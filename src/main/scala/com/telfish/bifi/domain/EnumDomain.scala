package com.telfish.bifi
package domain

import collection.mutable.ArrayBuffer

class EnumDomain[T](values: IndexedSeq[T]) extends RangeDomain[T, SetExpr[T]] {
  def elementAt(pos: Long): T = values(pos.toInt)
  def indexOf(t: T): Long = values.indexOf(t)
  def size: Long = values.size

  val Size = size
  def rangeify(range: (Long, Long)): List[SetExpr[T]] = List(range match {
    case (0, Size)            => SetExpr.All
    case (a, b) if a + 1 == b => SetExpr.Single(elementAt(a))
    case (a, b) if a < b      => SetExpr.Several((a until b) map elementAt toSet)
  })

  def range(expr: SetExpr[T]): List[(Long, Long)] = expr match {
    case SetExpr.Single(x)   => List(indexRange(x, x))
    case SetExpr.Several(xs) =>
      var curStart = -2L
      var last = -2L
      val res = new ArrayBuffer[(Long, Long)]
      xs.map(indexOf).toList.sorted.foreach { idx =>
        if (idx == last + 1L) {
          last = idx
        } else {
          if (curStart >= 0L)
            res += ((curStart, last + 1L))
          curStart = idx
          last = idx
        }
      }
      if (curStart >= 0L)
        res += ((curStart, last + 1L))
      res.toList
    case SetExpr.All         => List((0, size))
  }

  import SetExpr._
  private def mergeTwo(first: SetExpr[T], second: SetExpr[T]): SetExpr[T] = {
    def newSeveral(s: Set[T]): SetExpr[T] =
      if (s.size == Size)
        All
      else
        Several(s)

    (first, second) match {
      case (a, b) if a == b => a
      case (Single(a), Single(b)) =>
        if (a == b)
          Single(a)
        else
          newSeveral(Set(a, b))

      case (Single(a), Several(others)) => newSeveral(others + a)
      case (Several(others), Single(a)) => newSeveral(others + a)

      case (Several(as), Several(bs)) =>
        newSeveral(as union bs)

      case (a, b) if a == All || b == All => All

      case _ => throw new IllegalStateException("Shouldn't happen")
    }
  }

  def mergeRanges(ranges: List[SetExpr[T]]): List[SetExpr[T]] = ranges match {
    case Nil         => Nil
    case one::Nil    => one::Nil
    case first::rest => mergeRanges(rest) map (mergeTwo(_, first))
  }
}

