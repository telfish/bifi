package com.telfish.bifi.domain

import org.scalacheck.{Gen, Arbitrary}

trait ExampleDomains {
  case class SimpleDomain(override val size: Long) extends NumericRangeDomain[Char]('a', ('a' + size).toChar) {
    type V = ExampleDomains#Value[Char, this.type]
  }

  val domainA = SimpleDomain(2)
  val domainB = SimpleDomain(3)
  val domainC = SimpleDomain(4)

  val theDomain = domainA × domainB × domainC

  val longOrder = implicitly[Ordering[Long]]
  case class Value[T, D <: Domain[T]](domain: D, get: T) extends Ordered[Value[T, D]] {
    def index: Long = domain.indexOf(get)

    def compare(that: Value[T, D]): Int = longOrder.compare(this.index, that.index)
  }

  def arbitraryValue[T](implicit d: Domain[T]): Arbitrary[Value[d.Value, d.type]] =
    Arbitrary(Gen.oneOf(d.values).map(v => Value[d.Value, d.type](d, v)))

  val a = 'a'
  val b = 'b'
  val c = 'c'
  val d = 'd'
}