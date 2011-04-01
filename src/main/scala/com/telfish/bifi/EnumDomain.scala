package com.telfish.bifi

import collection.mutable.ArrayBuffer

sealed trait SetExpr[+T]

object SetExpr {
  case class Single[T](t: T) extends SetExpr[T]
  case class Several[T](t: List[T]) extends SetExpr[T]
  case object All extends SetExpr[Nothing]

  trait HasTuplizer[X] {
    def ~[U](next: U): (X, U)
  }

  implicit def tuplize[T](x: T): HasTuplizer[T] = new HasTuplizer[T] {
    def ~[U](next: U): (T, U) = (x, next)
  }
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
    case SetExpr.Several(xs) =>
      var curStart = -2L
      var last = -2L
      val res = new ArrayBuffer[(Long, Long)]
      xs.map(indexOf).sorted foreach { idx =>
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
}

class HashedEnumDomain[T](values: IndexedSeq[T]) extends EnumDomain(values) {
  val elementToIndexMap = {
    val map = new java.util.IdentityHashMap[T, Int]
    values.zipWithIndex foreach { case (v, i) => map.put(v, i) }
    map
  }

  override def indexOf(t: T): Long = elementToIndexMap.get(t)
}