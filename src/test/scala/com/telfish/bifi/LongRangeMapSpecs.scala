package com.telfish.bifi

import org.scalacheck.{Gen, Prop}
import Prop._
import org.specs.{ScalaCheck, Specification}

object LongRangeMapSpecs extends Specification with ScalaCheck with BetterScalaCheckVerifies {
  "LongRangeMap" should {
    "insert elements and find them later on" in {
      "contains only elements that were inserted" verifies { (single: Long, other: Long) =>
        val map = Builder[Long].add(single, single + 1, single).toLongRangeMap

        ((other != single) ==> map.get(other).isEmpty) || ((other == single) ==> map.get(other).exists(_ == single))
      }

      "work correctly with ranges" verifies { (start: Long, length: Long) =>
        val end = start + length
        val map = Builder[String].add(start, end, "test").toLongRangeMap

        Prop.forAll(Gen.choose(start, end - 1))(candidate => map.get(candidate).isDefined)
      }

      "manual tests" in {
        val map = Builder[String].add(50, 100, "test").add(102, 110, "test2").toLongRangeMap

        map.get(85) must beSome("test")
        map.get(100) must beNone
        map.get(105) must beSome("test2")
        map.get(111) must beNone
      }

      "find gaps" in {
        val map = Builder[String].add(50, 100, "test").add(102, 110, "test2").toLongRangeMap

        "in between" in {
          map.gaps(80, 110) must be_==(List((100, 102)))
        }

        "at start and end" in {
          map.gaps(0, 200) must be_==(List((0, 50), (100, 102), (110, 200)))
        }
      }
    }

    "find overlaps" in {
      val builder = Builder[String].add(50, 100, "test").add(99, 5, "test2").toLongRangeMap

      builder.overlaps(2, 39) must beEmpty
      builder.overlaps(95, 110) must be_==(List((99, 100, List("test", "test2"))))
    }
  }

  def Builder[A: ClassManifest]: LongRangeMapBuilder[A] = new LongRangeMapBuilder[A]
}