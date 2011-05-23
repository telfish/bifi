package com.telfish.bifi

import java.util.Arrays
import collection.mutable.{ListBuffer, ArrayBuffer}
import annotation.elidable

object Output {
  var last: Long = -1
  @elidable(elidable.FINEST)
  def tick(str: String) = {
    val now = System.currentTimeMillis
    if (last != -1)
      println("+%5d %10d %s" format (now - last, now, str))
    else
      println("       %10d %s" format (now, str))

    last = now
  }
}
import Output.tick

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
  def ++[B >: A: ClassManifest](other: LongRangeMap[B]): LongRangeMap[B]

  def traverse: Traversable[(Long, Long, A)]
  def traverseEntries: Traversable[Entry[A]]

  def map[B: ClassManifest](f: A => B): LongRangeMap[B]
}

trait GenericRLELongRangeMap[A] extends LongRangeMap[A]{
  def valueAt(i: Int): A
  def starts: Array[Long]
  protected def lengths: Array[Long]
  implicit def manifest: ClassManifest[A]

  def values: Seq[A] = (0 until cardinality) map valueAt

  def size = starts.length

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

      tick("In normalize.foreach")

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

        if (starts(i + 1) < ends(i)) {
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
        }
        else {
          add(starts(i), ends(i), merge(List(valueAt(i))))
          i += 1
        }
        curEnd = thisEnd
      }

      if (i < size)
        add(math.max(curEnd, starts(i)), ends(i), merge(List(valueAt(i))))

      tick("After normalize.foreach")
    }
  }

  def cardinality: Int = size

  def |[B: ClassManifest](other: LongRangeMap[B]): Traversable[(Long, Long, (Option[A], Option[B]))] = other match {
    case other: GenericRLELongRangeMap[_] =>
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

      tick("Before merging")

      while (resIdx < newSize) {
        if (bIdx >= other.starts.length || (aIdx < starts.length && starts(aIdx) <= other.starts(bIdx)))
          takeA
        else
          takeB

        resIdx += 1
      }

      tick("After merging")

      val intermediary = new Tuple2OptionRLELongRangeMap(mergedStarts, mergedLengths, mergedAValues, mergedBValues)

      tick("before normalizing")

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

  def ++[B >: A: ClassManifest](other: LongRangeMap[B]): LongRangeMap[B] = other match {
    case other: GenericRLELongRangeMap[_] =>

      val newSize = starts.length + other.starts.length

      val mergedStarts = new Array[Long](newSize)
      val mergedLengths = new Array[Long](newSize)
      val mergedValues = new Array[B](newSize)

      var aIdx   = 0
      var bIdx   = 0
      var resIdx = 0

      def takeA = {
        mergedStarts (resIdx) = starts(aIdx)
        mergedLengths(resIdx) = lengths(aIdx)
        mergedValues (resIdx) = valueAt(aIdx)

        aIdx += 1
      }
      def takeB = {
        mergedStarts (resIdx) = other.starts(bIdx)
        mergedLengths(resIdx) = other.lengths(bIdx)
        mergedValues (resIdx) = other.valueAt(bIdx)

        bIdx += 1
      }

      tick("Before merging")

      while (resIdx < newSize) {
        if (bIdx >= other.starts.length || (aIdx < starts.length && starts(aIdx) <= other.starts(bIdx)))
          takeA
        else
          takeB

        resIdx += 1
      }

      tick("After merging")

      new RLELongRangeMap(mergedStarts, mergedLengths, mergedValues)
    case _ => throw new UnsupportedOperationException("| only supported with other RLELongRangeMaps")
  }

  def traverse: Traversable[(Long, Long, A)] =
    (0 until cardinality).view map (i => (starts(i), ends(i), valueAt(i)))

  def traverseEntries: Traversable[Entry[A]] =
    (0 until cardinality).view map (i => Entry(starts(i), lengths(i), valueAt(i)))

  def map[B: ClassManifest](f: A => B): LongRangeMap[B] = new RLELongRangeMap(starts, lengths, values map f toArray)
}

abstract class GenericRLELongRangeMapImpl[A](val starts: Array[Long], protected val lengths: Array[Long])(implicit mf: ClassManifest[A]) extends GenericRLELongRangeMap[A] {
  implicit def manifest: ClassManifest[A] = mf

  override val size = starts.length
}

class RLELongRangeMap[A: ClassManifest](starts: Array[Long], lengths: Array[Long], values: Array[A]) extends GenericRLELongRangeMapImpl[A](starts, lengths) {
  def valueAt(i: Int): A = values(i)
}
object RLELongRangeMap {
  def fromSortedEntries[A: ClassManifest](entries: Iterable[Entry[A]]): RLELongRangeMap[A] = {
    tick("Starting fromSortedEntries")
    val size = entries.size
    val starts  = new Array[Long](size)//entries.view.map(_.start).toArray
    val lengths = new Array[Long](size)//entries.view.map(_.length).toArray
    val values  = new Array[A](size)//entries.view.map(_.value).toArray

    var i = 0

    entries foreach { e =>
      starts(i) = e.start
      lengths(i) = e.length
      values(i) = e.value

      i += 1
    }
    tick("After fromSortedEntries created arrays")

    new RLELongRangeMap(starts, lengths, values)
  }
}

class Tuple2OptionRLELongRangeMap[A, B](starts: Array[Long], lengths: Array[Long], valuesA: Array[A], valuesB: Array[B]) extends GenericRLELongRangeMapImpl[(Option[A], Option[B])](starts, lengths) {
  def valueAt(i: Int): (Option[A], Option[B]) = (Option(valuesA(i)), Option(valuesB(i)))
}

case class Entry[+A](start: Long, length: Long, value: A)
class LongRangeMapBuilder[A: ClassManifest] {

  var entries = new ArrayBuffer[Entry[A]]

  def add(from: Long, to: Long, value: A): this.type = {
    assert(from < to)

    entries += Entry(from, to - from, value)
    this
  }

  def toLongRangeMap: LongRangeMap[A] = {
    val sorted = entries.sortBy(_.start)

    RLELongRangeMap.fromSortedEntries(sorted)
  }

  def integrateInto(multiMap: LongRangeMultiMap)(implicit ev: A <:< AnyRef): LongRangeMap[A] = multiMap.integrate[AnyRef](entries.asInstanceOf[Seq[Entry[AnyRef]]]).asInstanceOf[LongRangeMap[A]]
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