package com.telfish.bifi

import java.util.Arrays
import collection.mutable.{ListBuffer, ArrayBuffer}

/**
 * A long range map maps a long value to an arbitrary value.
 */
trait LongRangeMap[+A] {
  /**
   * Gets the value associated with a long value
   */
  def get(l: Long): Option[A]

  /**
   * The number of ranges in this map
   */
  def cardinality: Int

  /**
   * Find all gaps in the definition space between 0 and end
   */
  def gaps(end: Long): List[(Long,  Long)]

  /**
   * Find all double definitions in the definition space.
   */
  def overlaps: List[(Long, Long, List[A])]

  protected[bifi] def normalize[B](merge: List[A] => B): Traversable[(Long, Long, B)]

  /**
   * Similar to ×. Returns a long range map where
   *  get(l) == (this.get(l), other.get(l))
   */
  def |[B: ClassManifest](other: LongRangeMap[B]): Traversable[(Long, Long, (Option[A], Option[B]))]

  def traverse: Traversable[(Long, Long, A)]
}

abstract class GenericRLELongRangeMap[A: ClassManifest](protected val starts: Array[Long], protected val lengths: Array[Long]) extends LongRangeMap[A]{
  def valueAt(i: Int): A

  val size = starts.length

  val ends = new IndexedSeq[Long] {
    def length: Int = starts.length
    def apply(idx: Int): Long = starts(idx) + lengths(idx)
  }

  def indexAt(l: Long): Option[Int] = {
    val index = Arrays.binarySearch(starts, l)

    if (index >= 0)
      Some(index)
    else {
      val insertPoint = - index - 1

      if (insertPoint == 0) // there is no entry before the entry in question
        None
      else if (l - starts(insertPoint - 1) < lengths(insertPoint -1))
        Some(insertPoint - 1)
      else
        None
    }
  }

  def get(l: Long): Option[A] =
    indexAt(l).map(valueAt)

  def gaps(end: Long): List[(Long, Long)] = {
    val buffer = new ListBuffer[(Long, Long)]

    var i = -1

    while (i < size) {
      val thisEnd = if (i >= 0) ends(i) else 0L
      val nextStart = if (i + 1 < size) starts(i + 1) else end

      if (thisEnd < nextStart)
        buffer += ((thisEnd, nextStart))

      i += 1
    }

    buffer.toList
  }


  def overlaps: List[(Long, Long, List[A])] =
    normalize(identity).filter(_._3.size > 1).toList

  /**
   * In a LongRangeMap with overlapping ranges, consolidate
   * double definitions with a merge function.
   */
  def normalize[B](merge: List[A] => B): Traversable[(Long, Long, B)] = new Traversable[(Long, Long, B)] {
    def foreach[U](f: ((Long, Long, B)) => U) {
      def add(start: Long, end: Long, value: B): Unit = {
        assert(start <= end)

        if (start < end)
          f((start, end, value))
      }


      /*
       * The normalization strategy here is this:
       *  - go forward through the list of intervals
       *  - for each interval first look, how many of the directly following intervals lap into the current one
       *  - with this list of overlapping intervals do the following:
       *    * collect a set of events, events are all starts and ends of the intervals in question
       *    * order this set
       *    * slide over the events and find all active events inside the interval
       *    * if more than one element interval was found active inside an interval, report it
       */

      val size = starts.size
      var i = 0
      var curEnd = 0L

      while (i + 1 < size) {
        import FindHelper._

        val thisEnd = ends(i)

        val nextStart = starts.indexWhere(_ >= thisEnd, i + 1)
        val endIndex = nextStart foundOrElse size

        val overlappingIdxs = (i until endIndex)
        val events =
          (overlappingIdxs
            .flatMap(idx => List(starts(idx), ends(idx)))
            .toSet
            .filter(_ <= thisEnd)
            .toList
            .sorted)

        events.sliding(2) foreach { case List(start, end) =>
          val active = overlappingIdxs filter (idx => (starts(idx) <= start) && (ends(idx) >= end))
          assert (active.size > 0)

          add(math.max(curEnd, start), end, merge(active map valueAt toList))
        }

        i = ends.indexWhere(_ > thisEnd, i + 1) foundOrElse size
        curEnd = thisEnd
      }

      if (i < size)
        add(math.max(curEnd, starts(i)), ends(i), merge(List(valueAt(i))))
    }
  }

  def cardinality: Int = size

  def |[B: ClassManifest](other: LongRangeMap[B]): Traversable[(Long, Long, (Option[A], Option[B]))] = other match {
    case other: GenericRLELongRangeMap[B] =>
      // the strategy here is:
      // 1.) simply merge both maps
      // 2.) create an intermediary map
      // 3.) normalize this intermediary map
      // 4.) create the final map using the results from normalization

      val newSize = starts.length + other.starts.length

      val mergedStarts = new Array[Long](newSize)
      val mergedLengths = new Array[Long](newSize)
      val mergedAValues = new Array[A](newSize)
      val mergedBValues = new Array[B](newSize)

      var aIdx   = 0
      var bIdx   = 0
      var resIdx = 0

      def takeA = {
        mergedStarts (resIdx) = starts(aIdx)
        mergedLengths(resIdx) = lengths(aIdx)
        mergedAValues(resIdx) = valueAt(aIdx)

        aIdx += 1
      }
      def takeB = {
        mergedStarts (resIdx) = other.starts(bIdx)
        mergedLengths(resIdx) = other.lengths(bIdx)
        mergedBValues(resIdx) = other.valueAt(bIdx)

        bIdx += 1
      }

      while (resIdx < newSize) {
        if (bIdx >= other.starts.length || (aIdx < starts.length && starts(aIdx) <= other.starts(bIdx)))
          takeA
        else
          takeB

        resIdx += 1
      }

      val intermediary = new Tuple2OptionRLELongRangeMap(mergedStarts, mergedLengths, mergedAValues, mergedBValues)

      val normalized =
        intermediary.normalize {
          case element :: Nil                            => element
          case (Some(a), None) :: (None, Some(b)) :: Nil => (Some(a), Some(b))
          case (None, Some(b)) :: (Some(a), None) :: Nil => (Some(a), Some(b))
          // TODO: we have overlaps in the definition spaces of the map
          // for now we just ignore most of the stuff, think about what todo here
          case element :: _ => element
          case _            => throw new IllegalStateException("May not happen by definition of normalize")
        }

      normalized

    case _ => throw new UnsupportedOperationException("| only supported with other RLELongRangeMaps")
  }

  def traverse: Traversable[(Long, Long, A)] =
    (0 until cardinality).view map (i => (starts(i), ends(i), valueAt(i)))
}

class RLELongRangeMap[A: ClassManifest](starts: Array[Long], lengths: Array[Long], values: Array[A]) extends GenericRLELongRangeMap[A](starts, lengths) {
  def valueAt(i: Int): A = values(i)
}
object RLELongRangeMap {
  def fromSortedEntries[A: ClassManifest](entries: Traversable[(Long, Long, A)]): RLELongRangeMap[A] = {
    val starts  = entries.view.map(_._1).toArray
    val lengths = entries.view.map(_._2).toArray
    val values  = entries.view.map(_._3).toArray

    new RLELongRangeMap(starts, lengths, values)
  }
}

class Tuple2OptionRLELongRangeMap[A, B](starts: Array[Long], lengths: Array[Long], valuesA: Array[A], valuesB: Array[B]) extends GenericRLELongRangeMap[(Option[A], Option[B])](starts, lengths) {
  def valueAt(i: Int): (Option[A], Option[B]) = (Option(valuesA(i)), Option(valuesB(i)))
}

class LongRangeMapBuilder[A: ClassManifest] {
  type Entry = (Long, Long, A)
  var entries = new ArrayBuffer[Entry]

  def add(from: Long, to: Long, value: A): this.type = {
    assert(from < to)

    entries += ((from, to - from, value))
    this
  }

  def toLongRangeMap: LongRangeMap[A] = {
    val sorted = entries.sortBy(_._1)

    RLELongRangeMap.fromSortedEntries(sorted)
  }
}

object FindHelper {
  trait FoundOrElse {
    def foundOrElse(elseIdx: Int): Int
  }
  implicit def int2FoundOrElse(searchResult: Int): FoundOrElse =
    new FoundOrElse {
      def foundOrElse(elseIdx: Int): Int =
        if (searchResult == -1) elseIdx else searchResult
    }
}