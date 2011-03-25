package com.telfish.bifi

import org.specs.{ScalaCheck, Specification}
import org.scalacheck.{Gen, Arbitrary}
import org.specs.specification.Example

object DomainSpecs extends Specification with ScalaCheck /*with BetterScalaCheckVerifies*/ {
  case class SimpleDomain(size: Long) extends Domain[Char] {
    def elementAt(pos: Long): Char = ('a' + pos).toChar
    def indexOf(t: Char): Long = t - 'a'
  }

  case class Value[T, D <: Domain[T]](get: T)
  def arbitraryValue[T](implicit d: Domain[T]): Arbitrary[Value[d.Value, d.type]] =
    Arbitrary(Gen.oneOf(d.values).map(Value.apply _))

  val a = 'a'
  val b = 'b'
  val c = 'c'
  val d = 'd'

  val domainA = SimpleDomain(2)
  val domainB = SimpleDomain(3)
  val domainC = SimpleDomain(4)

  "Domains" should {
    "properly span up a 2-dim space" in {
      val myDomain = Domain.tupled2Domain(domainA, domainB)
      checkIdentities(myDomain)

      "manual examples" in {
        myDomain.elementAt(0) must be_==((a, a))
        myDomain.elementAt(1) must be_==((a, b))
        myDomain.elementAt(2) must be_==((a, c))
        myDomain.elementAt(3) must be_==((b, a))
        myDomain.elementAt(4) must be_==((b, b))
        myDomain.elementAt(5) must be_==((b, c))
      }

      "calculate proper ranges" in {
        val range = myDomain.range((a, a), (b, a))

        range must be_==(List((0, 1), (3, 4)))
      }
    }
    "properly span up a 3-dim space" in {
      val myDomain = Domain.tupled3Domain(SimpleDomain(2), SimpleDomain(3), SimpleDomain(4))

      checkIdentities(myDomain)

      "manual examples" in {
        myDomain.elementAt(0) must be_==((a, a, a))
        myDomain.elementAt(1) must be_==((a, a, b))
        myDomain.elementAt(2) must be_==((a, a, c))
        myDomain.elementAt(3) must be_==((a, a, d))
        myDomain.elementAt(4) must be_==((a, b, a))
        myDomain.elementAt(5) must be_==((a, b, b))
        myDomain.elementAt(6) must be_==((a, b, c))
        myDomain.elementAt(7) must be_==((a, b, d))
        myDomain.elementAt(8) must be_==((a, c, a))
        myDomain.elementAt(9) must be_==((a, c, b))
        myDomain.elementAt(10) must be_==((a, c, c))
        myDomain.elementAt(11) must be_==((a, c, d))
        myDomain.elementAt(12) must be_==((b, a, a))
      }

      "calculate proper ranges" in {
        val range = myDomain.range((a, a, a), (b, b, b))

        range must be_==(List((0, 2), (4, 6), (12, 14), (16, 18)))
      }
    }
  }

  def checkIdentities[T](myDomain: Domain[T]) = {
    implicit val x = arbitraryValue(myDomain)

    "elementAt(indexOf) must be identity" verifies { (a: Value[myDomain.Value, myDomain.type]) =>
      myDomain.elementAt(myDomain.indexOf(a.get)) == a.get
    }

    "indexOf(elementAt) must be identity" in {
      val allValues = Gen.choose(0L, myDomain.size-1)

      allValues must pass { (index: Long) =>
        myDomain.indexOf(myDomain.elementAt(index)) == index
      }
    }
  }
}