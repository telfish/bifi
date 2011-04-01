package com.telfish.bifi
package domain

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
