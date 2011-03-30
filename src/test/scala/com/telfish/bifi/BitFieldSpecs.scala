package com.telfish.bifi

import org.specs.{ScalaCheck, Specification}
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

import org.junit.runner.RunWith

import org.scalacheck._
import Prop._

@RunWith(classOf[JUnitSuiteRunner])
class BitFieldSpecsJUnit extends JUnit4(BitFieldSpecs, DomainSpecs, LongRangeMapSpecs, DomainMapSpecs)

object BitFieldSpecs extends Specification("BitField") with ScalaCheck {
  "set bits must be set" verifies { (a: Long) =>
    BitField(a).isSet(a)
  }

  "all set bits must be set" verifies { (as: List[Long]) =>
    val field = BitField(as)
    as.forall(field.isSet(_))
  }

  "other bit than set mustn't be set" verifies { (as: List[Long], other: Long) =>
    val field = BitField(as)

    (!as.contains(other)) ==> !field.isSet(other)
  }

  "OR two fields" verifies { (as: List[Long], bs: List[Long]) =>
    val field1 = BitField(as)
    val field2 = BitField(bs)
    val combined = field1 | field2

    as.forall(combined.isSet(_)) :| "contains all elements from the left field" &&
    bs.forall(combined.isSet(_)) :| "contains all elements from the right field"
  }
}
