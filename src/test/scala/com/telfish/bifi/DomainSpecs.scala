package com.telfish.bifi

import org.specs.{ScalaCheck, Specification}
import org.scalacheck.{Prop, Gen}
import org.specs.specification.{Example, PendingUntilFixed}

object DomainSpecs extends Specification with ScalaCheck /*with BetterScalaCheckVerifies*/ with ExampleDomains with PendingUntilFixed {
  "Domains" should {
    "rangeify index ranges" in {
      import RangeExpr.{range => r, _}

      "all" in {
        domainC.rangeify(0, 4) must be_==(all)
      }
      "range" in {
        domainC.rangeify(1, 3) must be_==(r(b, c))
      }
      "single" in {
        domainC.rangeify(1, 2) must be_==(single(b))
      }
      "multiple" in {

      } pendingUntilFixed

      null: Example
    }

    "properly span up a 2-dim space" in {
      val myDomain = domainA × domainB

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
        import RangeExpr.{range => r, _}
        val range = myDomain.range((r(a, b), single(a)))

        range must be_==(List((0, 1), (3, 4)))
      }
    }
    "properly span up a 3-dim space" in {
      val myDomain = domainA × domainB × domainC // map ({ case ((a, b), c) => (a, b, c) }, { case (a, b, c) => ((a, b), c) })

      checkIdentities(myDomain)

      def t(a: Char, b: Char, c: Char) = ((a, b), c)

      "manual examples" in {
        myDomain.elementAt(0) must be_==(t(a, a, a))
        myDomain.elementAt(1) must be_==(t(a, a, b))
        myDomain.elementAt(2) must be_==(t(a, a, c))
        myDomain.elementAt(3) must be_==(t(a, a, d))
        myDomain.elementAt(4) must be_==(t(a, b, a))
        myDomain.elementAt(5) must be_==(t(a, b, b))
        myDomain.elementAt(6) must be_==(t(a, b, c))
        myDomain.elementAt(7) must be_==(t(a, b, d))
        myDomain.elementAt(8) must be_==(t(a, c, a))
        myDomain.elementAt(9) must be_==(t(a, c, b))
        myDomain.elementAt(10) must be_==(t(a, c, c))
        myDomain.elementAt(11) must be_==(t(a, c, d))
        myDomain.elementAt(12) must be_==(t(b, a, a))
      }

      "calculate proper range indices" in {
        import RangeExpr.{range => r, _}

        "for ranges" in {
          val range = myDomain.range(((r(a, b), r(a, b)), r(a, b)))

          range must be_==(List((0, 2), (4, 6), (12, 14), (16, 18)))
        }
        "for complete domains" in {
          val range = myDomain.range(((all, r(a, b)), all))

          range must be_==(List((0, 4), (4, 8), (12, 16), (16, 20)))
        }
      }

      "calculate proper range expr from range indices" in {
        import RangeExpr.{range => r, all, _}

        "for all" in {
          val expr = myDomain.rangeify((0, 24))

          expr must be_==(((all, all), all))
        }

        "for parts" in {
          val expr = myDomain.rangeify((9, 12))

          expr must be_==(((single(a), single(c)), r(b, d)))
        }

        import Prop._
        val validIndex = Gen.choose(0, myDomain.size - 1)

        "for arbitrary index ranges (roundtrip)" in {
          val roundTrip = Prop.forAll(validIndex, validIndex) { (start: Long, end: Long) =>
            (end > start) ==> (myDomain.range(myDomain.rangeify(start, end)) == List((start, end)))
          }
          roundTrip must pass
        }

        "for arbitrary ranges in parts (roundtrip)" in {
          implicit val x = arbitraryValue(domainA)

          val roundTrip = Prop.forAll { (start: Value[Char, domainA.type], end: Value[Char, domainA.type]) =>
            val range = ((all, all), r(start.get, end.get))

            end > start ==> (myDomain.rangeify(myDomain.range(range).head) == range)
          }
          roundTrip must pass
        }
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