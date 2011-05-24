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

    "normalize" in {
      "situation without overlaps" in {
        val map =
          Builder[String]
            .add("***       ", "test")
            .add("     *****", "test2")
            .toLongRangeMap

        map.normalize(identity).toList must be_==(List(
               t("***       ", List("test")),
               t("     *****", List("test2"))
        ))
      }
      "situation with single overlaps" in {
        val map =
          Builder[String]
            .add("******    ", "test")
            .add("    ******", "test2")
            .toLongRangeMap

        map.normalize(identity).toList must be_==(List(
               t("****      ", List("test")),
               t("    **    ", List("test", "test2")),
               t("      ****", List("test2"))
        ))
      }

      "situation with sequential overlaps" in {
        val map =
          Builder[String]
            .add("******          ", "test")
            .add("   *******      ", "test2")
            .add("         ****** ", "tester")
            .toLongRangeMap

        map.normalize(identity).toList must be_==(List(
               t("***             ", List("test")),
               t("   ***          ", List("test", "test2")),
               t("      ***       ", List("test2")),
               t("         *      ", List("test2", "tester")),
               t("          ***** ", List("tester"))
        ))
      }
    }

    "find overlaps" in {
      "test1" in {
        val map =
          Builder[String]
            .add("***************     ", "test")
            .add("     *****          ", "test2")
            .add("       *************", "tester")
            .toLongRangeMap

        map.overlaps must be_==(List(
               t("     **             ", List("test", "test2")),
               t("       ***          ", List("test", "test2", "tester")),
               t("          *****     ", List("test", "tester"))
        ))
      }

      "test2" in {
        val map =
          Builder[String]
            .add("   *", "test")
            .add("  **", "test2")
            .add("****", "tester")
            .toLongRangeMap

        map.overlaps must be_==(List(
               t("  * ", List("tester", "test2")),
               t("   *", List("tester", "test2", "test"))
        ))
      }

      "test3" in {
        val map =
          Builder[String]
            .add("******          ", "test")
            .add("   *******      ", "test2")
            .add("         ****** ", "tester")
            .toLongRangeMap

        map.overlaps must be_==(List(
               t("   ***          ", List("test", "test2")),
               t("         *      ", List("test2", "tester"))
        ))
      }
    }

    "`|` with another map" in {
      "to find gaps and too much defined elements" in {
        val map =
          Builder[String]
            .add("***                 ", "test")
            .add("     *****          ", "test2")
            .add("            ******* ", "tester")
            .toLongRangeMap

        val checkMap =
          Builder[String]
            .add("  ***************   ", "defined")
            .toLongRangeMap

        val defined = Some("defined")

        (map | checkMap).toList must be_==(List(
               t("**                  ", (Some("test"),   None)),
               t("  *                 ", (Some("test"),   defined)),
               t("   **               ", (None,           defined)),
               t("     *****          ", (Some("test2"),  defined)),
               t("          **        ", (None,           defined)),
               t("            *****   ", (Some("tester"), defined)),
               t("                 ** ", (Some("tester"), None))
        ))
      }
    }

    "`++` with another map" in {
        val map1 =
          Builder[String]
            .add("***                 ", "test")
            .toLongRangeMap

        val map2 =
          Builder[String]
            .add("            ***     ", "test2")
            .toLongRangeMap

        val defined = Some("defined")

        (map1 ++ map2).traverse.toList must be_==(List(
               t("***                 ", "test"),
               t("            ***     ", "test2")
        ))

        (map2 ++ map1).traverse.toList must be_==(List(
               t("***                 ", "test"),
               t("            ***     ", "test2")
        ))
    }
  }

  "A LongRangeMultiMap" should {
    "integrate two maps" in {
      "non-overlapping" in {
        val multiMap = LongRangeMultiMap.create("2-map")

        val map1 =
          Builder[String]
            .add("***                 ", "test")
            .integrateInto(multiMap)

        val map2 =
          Builder[String]
            .add("            ***     ", "test2")
            .integrateInto(multiMap)

        multiMap.optimize()

        map1.get(0) must beSome("test")
        map1.get(1) must beSome("test")
        map1.get(2) must beSome("test")

        map1.get(12) must beNone
        map2.get(0) must beNone

        map2.get(12) must beSome("test2")
        map2.get(13) must beSome("test2")
        map2.get(14) must beSome("test2")
      }
      "overlapping" in {
        val multiMap = LongRangeMultiMap.create("overlapping")

        val map1 =
          Builder[String]
            .add("***                 ", "test")
            .integrateInto(multiMap)

        val map2 =
          Builder[String]
            .add("  *****             ", "test2")
            .add("          ***       ", "test3")
            .integrateInto(multiMap)

        multiMap.optimize()

        map1.get(0) must beSome("test")
        map1.get(1) must beSome("test")
        map1.get(2) must beSome("test")

        map1.get(12) must beNone
        map2.get(0) must beNone

        map2.get(2) must beSome("test2")
        map2.get(3) must beSome("test2")
        map2.get(4) must beSome("test2")
        map2.get(5) must beSome("test2")
        map2.get(6) must beSome("test2")
        map2.get(7) must beNone

        map2.get(10) must beSome("test3")
        map2.get(11) must beSome("test3")
        map2.get(12) must beSome("test3")
      }
    }
    "integrate 3 maps" in {
      val multiMap = LongRangeMultiMap.create("3-map")

      val map1 =
        Builder[String]
          .add("***                 ", "test")
          .integrateInto(multiMap)

      val map2 =
        Builder[String]
          .add("            ***     ", "test2")
          .integrateInto(multiMap)

      multiMap.optimize()

      val map3 =
        Builder[String]
          .add(" **************     ", "test3")
          .integrateInto(multiMap)

      multiMap.optimize()
      multiMap.dump

      map1.get(0) must beSome("test")
      map1.get(1) must beSome("test")
      map1.get(2) must beSome("test")

      map1.get(12) must beNone
      map2.get(0) must beNone

      map2.get(12) must beSome("test2")
      map2.get(13) must beSome("test2")
      map2.get(14) must beSome("test2")

      map3.get(0) must beNone
      map3.get(1) must beSome("test3")
      map3.get(2) must beSome("test3")
      map3.get(3) must beSome("test3")
      map3.get(4) must beSome("test3")
      map3.get(5) must beSome("test3")
      map3.get(6) must beSome("test3")
      map3.get(7) must beSome("test3")
      map3.get(8) must beSome("test3")
      map3.get(9) must beSome("test3")
      map3.get(10) must beSome("test3")
      map3.get(11) must beSome("test3")
      map3.get(12) must beSome("test3")
      map3.get(13) must beSome("test3")
      map3.get(14) must beSome("test3")
      map3.get(15) must beNone
      map3.get(16) must beNone
    }
  }

  def Builder[A: ClassManifest]: GraphicBuilder[A] = new GraphicBuilder[A]

  val GraphicInterval = """([ ]*)(\**)[ ]*""".r
  def graphicInterval(str: String): (Long, Long) = str match {
    case GraphicInterval(whitespace, dashes) => (whitespace.size, whitespace.size + dashes.size)
  }
  def t[A](str: String, a: A): (Long, Long, A) = {
    val (start, end) = graphicInterval(str)
    (start, end, a)
  }

  class GraphicBuilder[A: ClassManifest] extends LongRangeMapBuilder[A] {
    def add(str: String, value: A): this.type = {
      val (start, end) = graphicInterval(str)
      add(start, end, value)
    }
  }
}