package com.telfish.bifi.domain

class NumericRangeDomain[T](start: T, end: T)(implicit num: Numeric[T]) extends RangeDomain[T, RangeExpr[T]] {
  import num._

  def elementAt(pos: Long): T = start + num.fromInt(pos.toInt)

  def indexOf(t: T): Long = (t - start).toLong

  def size: Long = (end - start).toLong

  def range(expr: RangeExpr[T]): List[(Long, Long)] = RangeExpr.rangeByExpr(expr, this)
  def rangeify(range: (Long, Long)): List[RangeExpr[T]] = List(RangeExpr.exprByRange(range, this))

  import RangeExpr._

  private def ordered(first: RangeExpr[T], second: RangeExpr[T]): (RangeExpr[T], RangeExpr[T]) = first match {
    case All => (second, first)
    case Single(_) => (first, second)
    case Range(_, _) => if (second.isInstanceOf[Single[_]]) (second, first) else (first, second)
  }

  private def mergeTwo(first: RangeExpr[T], second: RangeExpr[T]): List[RangeExpr[T]] = {
    def Range(a: T, b: T) =
      if (a == start && num.plus(b, num.one) == end)
        All
      else
        RangeExpr.Range(a, b)

    ordered(first, second) match {
      case (a, b) if a == b => List(a)
      case (a, b) if a == All | b == All => List(All)
      case (Single(a), Single(b)) if math.abs(indexOf(a) - indexOf(b)) == 1 => List(Range(num.min(a, b), num.max(a, b)))
      case (Single(a), Range(b, c)) if num.plus(a, num.one) == b      => List(Range(a, c))
      case (Single(a), Range(b, c)) if num.plus(c, num.one) == a      => List(Range(b, a))
      case (Range(a, b), Range(c, d)) if indexOf(b) + 1 == indexOf(c) => List(Range(a, d))
      case (Range(a, b), Range(c, d)) if indexOf(d) + 1 == indexOf(a) => List(Range(c, b))
      case (a, b) => List(a, b)
    }
  }

  def mergeRanges(ranges: List[RangeExpr[T]]): List[RangeExpr[T]] = ranges match {
    case Nil         => Nil
    case one::Nil    => one::Nil
    case first::rest => mergeRanges(rest) flatMap (mergeTwo(_, first))
  }
}