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
   * Find all gaps in the definition space between 0 and end
   */
  def gaps(end: Long): List[(Long,  Long)]

  /**
   * Find all double definitions in the definition space.
   */
  def overlaps: List[(Long, Long, List[A])]

  def cardinality: Int
}

abstract class GenericRLELongRangeMap[A](starts: Array[Long], lengths: Array[Long]) extends LongRangeMap[A]{
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
  def overlaps: List[(Long, Long, List[A])] = {
    /*
     * The strategy to find overlaps here is this:
     *  - go forward through the list of intervals
     *  - for each interval first look, how many of the directly following intervals lap into the current one
     *  - with this list of overlapping intervals do the following:
     *    * collect a set of events, events are all starts and ends of the intervals in question
     *    * order this set
     *    * slide over the events and find all active events inside the interval
     *    * if more than one element interval was found active inside an interval, report it
     */

    val buffer = new ListBuffer[(Long, Long, List[A])]

    var i = 0

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
        val active = overlappingIdxs filter (idx => starts(idx) <= start && ends(idx) >= end )

        if (active.size > 1)
          buffer += ((start, end, active map valueAt toList))
      }

      i = ends.indexWhere(_ > thisEnd, i + 1) foundOrElse size
    }
    buffer.toList
  }

  def cardinality: Int = size


case class RLELongRangeMap[A](starts: Array[Long], lengths: Array[Long], values: Array[A]) extends GenericRLELongRangeMap[A](starts, lengths) {
  def valueAt(i: Int): A = values(i)
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

    val starts = sorted.view.map(_._1).toArray
    val lengths = sorted.view.map(_._2).toArray
    val values = sorted.view.map(_._3).toArray

    new RLELongRangeMap(starts, lengths, values)
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