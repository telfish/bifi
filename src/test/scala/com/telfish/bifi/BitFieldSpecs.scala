package com.telfish.bifi

import org.specs.{ScalaCheck, Specification}
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

@RunWith(classOf[JUnitSuiteRunner])
class BitFieldSpecsJUnit extends JUnit4(BitFieldSpecs)

object BitFieldSpecs extends Specification with ScalaCheck {
  "contain set elements" verifies { (a: Long) =>
    BitField.empty.set(a).apply(a)
  }

  "contain each of several set elements" verifies { (as: List[Long]) =>
    val field = as.foldLeft(BitField.empty)(_.set(_))
    as.forall(field.isSet(_))
  }
}