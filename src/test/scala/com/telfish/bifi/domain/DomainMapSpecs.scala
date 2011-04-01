package com.telfish.bifi.domain

import org.specs.{ScalaCheck, Specification}
import org.specs.specification.PendingUntilFixed

object DomainMapSpecs extends Specification with ScalaCheck with ExampleDomains with PendingUntilFixed {
  noDetailedDiffs()
  import RangeExpr._

  "A DomainMap" should {
    "map domain values to target elements" in {
      "for single values" in {
        "manual test" in {
          val map = Builder[String].add(((Single(a), Single(b)), Single(a)), "test").toDomainMap

          map.get(((a, b), a)) must beSome("test")
        }

        implicit val arb = arbitraryValue(theDomain)
        "get the mapping" verifies { (a: Value[theDomain.Value, theDomain.type]) =>
          val map = Builder[String].addSingle(a.get, "test").toDomainMap

          map.get(a.get) == Some("test")
        }
      }
    }

    "report gaps" in {

      "if only single is set" in {
        val map = Builder[String].add(((Single(a), Single(b)), Single(a)), "test").toDomainMap

        map.gaps must containAll(List(
          ((Single(a), Single(b)), Range(b, d)),
          ((Single(a), Single(a)), All),
          ((Single(a), Single(c)), All),
          ((Single(b), All), All)
        ))
      } pendingUntilFixed

      "if several are set" in {
        val map =
          Builder[String]
            .add(((All, Range(a, b)), All), "test")
            .add(((All, Single(c)), Range(a, b)), "test2")
            .toDomainMap

        "simple" in {
          map.gaps must be_==(List(
            ((Single(a), Single(c)), Range(c, d)),
            ((Single(b), Single(c)), Range(c, d))
          ))
        }

        "joined" in {
          map.gaps must be_==(List(((All, Single(c)), Range(c, d))))
        } pendingUntilFixed("one of 'joined' or 'simple' cannot succeed, for now unification has not been implemented")
      }
    }
    "report overlaps" in {
      "single overlap" in {
        val map =
          Builder[String]
            .addSingle(((b, c), d), "test")
            .add(((Single(b), All), Range(c, d)), "test2")
            .toDomainMap

        map.overlaps must be_==(List((((Single(b), Single(c)), Single(d)), List("test2", "test"))))
      }

      "multiple overlaps" in {
        val map =
          Builder[String]
            .addSingle(((b, c), d), "test")
            .add(((Single(b), All), Range(c, d)), "test2")
            .add(((All, Single(c)), All), "tester")
            .toDomainMap

        map.overlaps must containAll(List(
          (((Single(b), Single(c)), Single(c)), List("tester", "test2")),
          (((Single(b), Single(c)), Single(d)), List("tester", "test2", "test"))
        ))
      }
    }
  }

  def Builder[A: ClassManifest]: DomainMapBuilder[theDomain.Value, theDomain.RangeT, A] = new DomainMapBuilder(theDomain)
}