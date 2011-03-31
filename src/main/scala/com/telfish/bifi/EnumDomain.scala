package com.telfish.bifi

sealed trait SetExpr[+T]

object SetExpr {
  case class Single[T](t: T) extends SetExpr[T]
  case class Several[T](t: List[T]) extends SetExpr[T]
  case object All extends SetExpr[Nothing]
}

class EnumDomain[T](values: IndexedSeq[T]) extends RangeDomain[T, SetExpr[T]] {
  def elementAt(pos: Long): T = values(pos.toInt)
  def indexOf(t: T): Long = values.indexOf(t)
  def size: Long = values.size

  val Size = size
  def rangeify(range: (Long, Long)): List[SetExpr[T]] = List(range match {
    case (0, Size)            => SetExpr.All
    case (a, b) if a + 1 == b => SetExpr.Single(elementAt(a))
    case (a, b) if a < b      => SetExpr.Several((a until b) map elementAt toList)
  })

  def range(expr: SetExpr[T]): List[(Long, Long)] = expr match {
    case SetExpr.Single(x)   => List(indexRange(x, x))
    case SetExpr.Several(xs) => xs map { x => indexRange(x, x) }
    case SetExpr.All         => List((0, size))
  }
}

class HashedEnumDomain[T](values: IndexedSeq[T]) extends EnumDomain(values) {
  val elementToIndexMap: Map[T, Int] = values.zipWithIndex.toMap

  override def indexOf(t: T): Long = elementToIndexMap(t)
}