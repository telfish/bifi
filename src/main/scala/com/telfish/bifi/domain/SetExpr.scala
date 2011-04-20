package com.telfish.bifi
package domain

sealed trait SetExpr[+T]

object SetExpr {
  case class Single[T](t: T) extends SetExpr[T]
  case class Several[T](t: Set[T]) extends SetExpr[T]
  object Several {
    def apply[T](els: T*): Several[T] = Several(els.toSet)
  }

  case object All extends SetExpr[Nothing]

  trait HasTuplizer[X] {
    def ~[U](next: U): (X, U)
  }

  implicit def tuplize[T](x: T): HasTuplizer[T] = new HasTuplizer[T] {
    def ~[U](next: U): (T, U) = (x, next)
  }

  def toString[T](name: String)(format: T => String)(expr: SetExpr[T]): String = expr match {
    case Single(l)   => format(l)
    case Several(ls) => ls.map(l => toString(name)(format)(Single(l))).mkString(", ")
    case All         => "all "+name
  }
}
