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

  def gaps: List[(Long,  Long)]

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

      def gaps: List[(Long, Long)] = {
        val buffer = new ListBuffer[(Long, Long)]

        var i = -1

        while (i + 1 < size) {
          val thisEnd = if (i >= 0) starts(i) + lengths(i) else 0L
          val nextStart = starts(i + 1)

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
          val thisEnd = starts(i) + lengths(i)

          val nextStart = starts.indexWhere(_ >= thisEnd, i + 1)
          val endIndex = if (nextStart == -1) size else nextStart

          if (endIndex > i + 1) {
            buffer += ((starts(i + 1), thisEnd, (i until endIndex) map values toList))
          }

          i += 1
        }
        buffer.toList
      }
    }
  }
}