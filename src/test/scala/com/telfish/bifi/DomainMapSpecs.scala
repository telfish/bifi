package com.telfish.bifi

import org.specs.{ScalaCheck, Specification}
import org.specs.specification.PendingUntilFixed

object DomainMapSpecs extends Specification with ScalaCheck with ExampleDomains with PendingUntilFixed {
  noDetailedDiffs()
  import RangeExpr._

  "A DomainMap" should {
    "map domain values to target elements" in {
      "for single values" in {
        "manual test" in {
          val map = Builder[String].add(((single(a), single(b)), single(a)), "test").toDomainMap

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
        val map = Builder[String].add(((single(a), single(b)), single(a)), "test").toDomainMap

        map.gaps must containAll(List(
          ((single(a), single(b)), range(b, d)),
          ((single(a), single(a)), all),
          ((single(a), single(c)), all),
          ((single(b), all), all)
        ))
      } pendingUntilFixed

      "if several are set" in {
        val map =
          Builder[String]
            .add(((all, range(a, b)), all), "test")
            .add(((all, single(c)), range(a, b)), "test2")
            .toDomainMap

        "simple" in {
          map.gaps must be_==(List(
            ((single(a), single(c)), range(c, d)),
            ((single(b), single(c)), range(c, d))
          ))
        }

        "joined" in {
          map.gaps must be_==(List(((all, single(c)), range(c, d))))
        } pendingUntilFixed("one of 'joined' or 'simple' cannot succeed, for now unification has not been implemented")
      }
    }
    "report overlaps" in {
      "single overlap" in {
        val map =
          Builder[String]
            .addSingle(((b, c), d), "test")
            .add(((single(b), all), range(c, d)), "test2")
            .toDomainMap

        map.overlaps must be_==(List((((single(b), single(c)), single(d)), List("test2", "test"))))
      }

      "multiple overlaps" in {
        val map =
          Builder[String]
            .addSingle(((b, c), d), "test")
            .add(((single(b), all), range(c, d)), "test2")
            .add(((all, single(c)), all), "tester")
            .toDomainMap

        map.overlaps must containAll(List(
          (((single(b), single(c)), single(c)), List("tester", "test2")),
          (((single(b), single(c)), single(d)), List("tester", "test2", "test"))
        ))
      }
    }
  }

  def Builder[A: ClassManifest]: DomainMapBuilder[theDomain.Value, theDomain.RangeT, A] = new DomainMapBuilder(theDomain)
}