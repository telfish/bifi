package com.telfish.bifi

import org.scalacheck.{Gen, Arbitrary}

trait ExampleDomains {
  case class SimpleDomain(size: Long) extends RangeDomain[Char, RangeExpr[Char]] {
    def elementAt(pos: Long): Char = ('a' + pos).toChar
    def indexOf(t: Char): Long = t - 'a'

    def range(expr: RangeExpr[Char]): List[(Long, Long)] = RangeExpr.rangeByExpr[Char](expr, this)
  }

  val domainA = SimpleDomain(2)
  val domainB = SimpleDomain(3)
  val domainC = SimpleDomain(4)

  val theDomain = domainA × domainB × domainC

  case class Value[T, D <: Domain[T]](get: T)
  def arbitraryValue[T](implicit d: Domain[T]): Arbitrary[Value[d.Value, d.type]] =
    Arbitrary(Gen.oneOf(d.values).map(Value.apply _))

  val a = 'a'
  val b = 'b'
  val c = 'c'
  val d = 'd'
}