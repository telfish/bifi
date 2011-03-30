package com.telfish.bifi

import java.util.Arrays
import collection.immutable.TreeSet
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
}

class LongRangeMapBuilder[A: ClassManifest] {
  type Entry = (Long, Long, A)
  var entries = new TreeSet[Entry]()(Ordering.by((_: Entry)._1))

  def add(from: Long, to: Long, value: A): this.type = {
    assert(from < to)

    entries += ((from, to - from, value))
    this
  }


  def toLongRangeMap: LongRangeMap[A] = {
    val sorted = entries

    val starts = sorted.view.map(_._1).toArray
    val lengths = sorted.view.map(_._2).toArray
    val values = sorted.view.map(_._3).toArray
    val size = sorted.size

    val ends = new IndexedSeq[Long] {
      def length: Int = size
      def apply(idx: Int): Long = starts(idx) + lengths(idx)
    }

    new LongRangeMap[A] {
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
        indexAt(l).map(values)

      def gaps(end: Long): List[(Long, Long)] = {
        val buffer = new ListBuffer[(Long, Long)]

        var i = -1

        while (i < size) {
          val thisEnd = if (i >= 0) starts(i) + lengths(i) else 0L
          val nextStart = if (i + 1 < size) starts(i + 1) else end

          if (thisEnd < nextStart)
            buffer += ((thisEnd, nextStart))

          i += 1
        }

        buffer.toList
      }
      def overlaps: List[(Long, Long, List[A])] = {
        val buffer = new ListBuffer[(Long, Long, List[A])]

        var i = 0

        while (i + 1 < size) {
          val thisEnd = ends(i)

          val nextStart = starts.indexWhere(_ >= thisEnd, i + 1)
          val endIndex = if (nextStart == -1) size else nextStart

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
              buffer += ((start, end, active map values toList))
          }

          i = endIndex
        }
        buffer.toList
      }
    }
  }
}