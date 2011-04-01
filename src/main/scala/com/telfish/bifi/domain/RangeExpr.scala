package com.telfish.bifi.domain

trait RangeExpr[+T]

object RangeExpr {
  case class Range[T](from: T, to: T) extends RangeExpr[T]
  case class Single[T](point: T) extends RangeExpr[T]
  case object All extends RangeExpr[Nothing]

  def rangeByExpr[T](expr: RangeExpr[T], domain: Domain[T]): List[(Long, Long)] = List(expr match {
    case Single(t) =>
      val index = domain.indexOf(t)
      (index, index + 1)
    case Range(from, to) =>
      (domain.indexOf(from), domain.indexOf(to) + 1)
    case All =>
      (0L, domain.size)
  })

  def exprByRange[T](range: (Long, Long), domain: Domain[T]): RangeExpr[T] = range match {
    case (a, b) if a + 1 == b       => Single(domain.elementAt(a))
    case (0, x) if x == domain.size => All
    case (a, b) if a < b            => Range(domain.elementAt(a), domain.elementAt(b - 1))
    case (a, b)                     => throw new IllegalStateException("invalid range: "+a+" - "+b)
  }
}