package com.telfish.bifi.domain

import org.specs.{ScalaCheck, Specification}
import org.scalacheck.{Prop, Gen}
import org.specs.specification.PendingUntilFixed

object DomainSpecs extends Specification with ScalaCheck with ExampleDomains with PendingUntilFixed {
  noDetailedDiffs()

  "Domains" should {
    import RangeExpr._

    "rangeify index ranges" in {

      "All" in {
        domainC.rangeify(0, 4) must be_==(List(All))
      }
      "range" in {
        domainC.rangeify(1, 3) must be_==(List(Range(b, c)))
      }
      "single" in {
        domainC.rangeify(1, 2) must be_==(List(Single(b)))
      }
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

      "calculate proper ranges indices" in {

        "if last one is single" in {
          val range = myDomain.range((Range(a, b), Single(a)))

          range must be_==(List((0, 1), (3, 4)))
        }
        "if last one is All" in {
          val range = myDomain.range((Range(a, b), All))

          range must be_==(List((0, 6)))
        }
      }

      "calculate proper range expr from range indices" in {
        "for ranges spanning branches at some level" in {
          "starting at the end of one branch" in {
            val expr = myDomain.rangeify((2, 5))

            expr must be_==(List(
              (Single(a), Single(c)),
              (Single(b), Range(a, b))
            ))
          }

          "starting at the start of one branch" in {
            val expr = myDomain.rangeify((0, 4))

            expr must be_==(List(
              (Single(a), All),
              (Single(b), Single(a))
            ))
          }
        }
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
        "for ranges" in {
          val range = myDomain.range(((Range(a, b), Range(a, b)), Range(a, b)))

          range must be_==(List((0, 2), (4, 6), (12, 14), (16, 18)))
        }
        "for complete domains" in {
          val range = myDomain.range(((All, Range(a, b)), All))

          range must be_==(List((0, 8), (12, 20)))
        }
      }

      "calculate proper range expr from range indices" in {
        "for All" in {
          val expr = myDomain.rangeify((0, 24))

          expr must be_==(List(((All, All), All)))
        }

        "for parts" in {
          val expr = myDomain.rangeify((9, 12))

          expr must be_==(List(((Single(a), Single(c)), Range(b, d))))
        }

        import Prop._
        val validIndex = Gen.choose(0, myDomain.size - 1)

        "for arbitrary index ranges (roundtrip)" in {
          val roundTrip = Prop.forAll(validIndex, validIndex) { (start: Long, end: Long) =>
            (end > start) ==> {
              val rs = myDomain.rangeify(start, end)
              (rs.size == 1) :| "only one range must be found" &&
                (myDomain.range(rs.head) == List((start, end))) :| "the correct range must be found"
            }
          }
          roundTrip must pass
        } pendingUntilFixed

        "for arbitrary ranges in parts (roundtrip)" in {
          implicit val x = arbitraryValue(domainA)

          val roundTrip = Prop.forAll { (start: Value[Char, domainA.type], end: Value[Char, domainA.type]) =>
            val range = ((All, All), Range(start.get, end.get))

            end > start ==> (myDomain.rangeify(myDomain.range(range).head) == range)
          }
          roundTrip must pass
        } pendingUntilFixed
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