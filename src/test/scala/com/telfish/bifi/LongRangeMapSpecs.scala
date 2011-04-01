package com.telfish.bifi

import org.specs.{ScalaCheck, Specification}
import org.scalacheck.{Arbitrary, Gen, Prop}
import Prop._

object LongRangeMapSpecs extends Specification with ScalaCheck {
  noDetailedDiffs()

  "LongRangeMap" should {
    "insert elements and find them later on" in {
      "contains only elements that were inserted" verifies { (single: Long, other: Long) =>
        val map = Builder[Long].add(single, single + 1, single).toLongRangeMap

        ((other != single) ==> map.get(other).isEmpty) || ((other == single) ==> map.get(other).exists(_ == single))
      }


      "work correctly with ranges" in {
        import Gen.choose
        import Arbitrary._

        val prop =
          Prop.forAll(arbitrary[Long], arbitrary[Long].suchThat(_ > 0)) { (start: Long, length: Long) =>
            val end = start + length
            val map = Builder[String].add(start, end, "test").toLongRangeMap

            Prop.forAll(choose(start, end - 1))(candidate => map.get(candidate).isDefined)
          }

        prop must pass
      }

      "manual tests" in {
        val map = Builder[String].add(50, 100, "test").add(102, 110, "test2").toLongRangeMap

        map.get(85) must beSome("test")
        map.get(100) must beNone
        map.get(105) must beSome("test2")
        map.get(111) must beNone
      }

      "find gaps" in {
        val map =
          Builder[String]
            .add(50, 100, "test")
            .add(102, 110, "test2")
            .toLongRangeMap

        map.gaps(120) must be_==(List((0, 50), (100, 102), (110, 120)))
      }
    }

    "find overlaps" in {
      "test1" in {
        val map =
          Builder[String]
            .add(50, 105, "test")
            .add(95, 100, "test2")
            .add(97, 110, "tester")
            .toLongRangeMap

        map.overlaps must be_==(List(
          (95, 97, List("test", "test2")),
          (97, 100, List("test", "test2", "tester")),
          (100, 105, List("test", "tester"))))
      }

      "test2" in {
        val map =
          Builder[String]
            .add(23, 24, "test")
            .add(22, 24, "test2")
            .add(20, 24, "tester")
            .toLongRangeMap

        map.overlaps must be_==(List((22, 23, List("tester", "test2")), (23, 24, List("tester", "test2", "test"))))
      }

      "test3" in {
        val map =
          Builder[String]
            .add(20, 23, "test")
            .add(22, 25, "test2")
            .add(24, 26, "tester")
            .toLongRangeMap

        map.overlaps must be_==(List(
          (22, 23, List("test", "test2")),
          (24, 25, List("test2", "tester"))
        ))
      }
    }
  }

  def Builder[A: ClassManifest]: LongRangeMapBuilder[A] = new LongRangeMapBuilder[A]
}