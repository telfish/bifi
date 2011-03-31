package com.telfish.bifi

class NumericRangeDomain[T](start: T, end: T)(implicit num: Numeric[T]) extends RangeDomain[T, RangeExpr[T]] {
  import num._

  def elementAt(pos: Long): T = start + num.fromInt(pos.toInt)

  def indexOf(t: T): Long = (t - start).toLong

  def size: Long = (end - start).toLong

  def range(expr: RangeExpr[T]): List[(Long, Long)] = RangeExpr.rangeByExpr(expr, this)
  def rangeify(range: (Long, Long)): List[RangeExpr[T]] = List(RangeExpr.exprByRange(range, this))
}