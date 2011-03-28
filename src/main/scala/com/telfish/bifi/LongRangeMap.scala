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
   * Reports the list of gaps between start and end.
   */
  def gaps(start: Long, end: Long): List[(Long,  Long)]

  def overlaps(from: Long, to: Long): List[(Long, Long, List[A])]
}

class LongRangeMapBuilder[A: ClassManifest] {
  type Entry = (Long, Long, A)
  var entries = new TreeSet[Entry]()(Ordering.by((_: Entry)._1))

  def add(from: Long, to: Long, value: A): this.type = {
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

      def gaps(start: Long, end: Long): List[(Long, Long)] = {
        indexAt(start) match {
          case Some(st) =>
            val buffer = new ListBuffer[(Long, Long)]

            var i = st
            var pos = start
            while (pos < end) {
              val thisEnd = starts(i) + lengths(i)
              val nextStart = if (i + 1 < size) starts(i + 1) else end

              if (thisEnd < nextStart)
                buffer += ((math.max(start, thisEnd), math.min(end, nextStart)))

              pos = nextStart
              i += 1
            }
            buffer.toList
          case None => (start, starts(0)) :: gaps(starts(0), end)
        }
      }
      def overlaps(from: Long, to: Long): List[(Long, Long, List[A])] = null
    }
  }
}