package com.telfish.bifi

import org.specs.{ScalaCheck, Specification}

object DomainMapSpecs extends Specification with ScalaCheck with ExampleDomains {
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

        map.gaps must be_==(List(
          ((single(a), single(b)), range(b, d)),
          ((single(a), single(a)), all),
          ((single(a), single(c)), all),
          ((single(b), all), all)
        ))
      }

      "if several are set" in {
        val map =
          Builder[String]
            .add(((all, range(a, b)), all), "test")
            .add(((all, single(c)), range(a, b)), "test2")
            .toDomainMap

        map.gaps must be_==(List(((all, single(c)), range(c, d))))
      }
    }
    "report overlaps" in {
      "single overlap" in {
        val map =
          Builder[String]
            .addSingle(((b, c), d), "test")
            .add(((single(b), all), range(c, d)), "test2")
            .toDomainMap

        map.overlaps must be_==(List((((single(b), single(c)), single(d)), List("test", "test2"))))
      }

      "multiple overlaps" in {
        val map =
          Builder[String]
            .addSingle(((b, c), d), "test")
            .add(((single(b), all), range(c, d)), "test2")
            .add(((all, single(c)), all), "test3")
            .toDomainMap

        map.overlaps must be_==(List((((single(b), single(c)), single(d)), List("test", "test2", "test3"))))
      }
    }
  }

  def Builder[A: ClassManifest]: DomainMapBuilder[theDomain.Value, theDomain.RangeT, A] = new DomainMapBuilder(theDomain)
}