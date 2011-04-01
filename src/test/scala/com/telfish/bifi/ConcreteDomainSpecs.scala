package com.telfish.bifi

import org.specs.{ScalaCheck, Specification}

object ConcreteDomainSpecs extends Specification with ScalaCheck {
  "EnumDomains" should {
    import SetExpr._

    sealed trait Day
    object Monday extends Day
    object Tuesday extends Day
    object Wednesday extends Day
    val Days = List(Monday, Tuesday, Wednesday)

    object DaysDomain extends HashedEnumDomain(Days.toIndexedSeq)

    "elementAt" in {
      DaysDomain.indexOf(Monday) must be_==(0L)
      DaysDomain.indexOf(Tuesday) must be_==(1L)
      DaysDomain.indexOf(Wednesday) must be_==(2L)
    }
    "indexOf" in {
      DaysDomain.elementAt(1L) must be_==(Tuesday)
    }
    "size" in {
      DaysDomain.size must be_==(3)
    }
    "range" in {
      DaysDomain.range(All) must be_==(List((0, 3)))
      DaysDomain.range(Several(List(Monday, Wednesday))) must be_==(List((0, 1), (2, 3)))
    }
    "rangeify" in {
      DaysDomain.rangeify((1, 3)) must be_==(List(Several(List(Tuesday, Wednesday))))
    }

    "simplify several single consecutive elements" in {
      DaysDomain.range(Several(List(Monday, Tuesday, Wednesday))) must be_==(List((0, 3)))
    }
  }

  "NumericRangeDomains" should {
    object MinuteDomain extends NumericRangeDomain(0, 60)

    "elementAt" in {
      MinuteDomain.indexOf(20) must be_==(20L)
    }
    "indexOf" in {
      MinuteDomain.elementAt(16L) must be_==(16)
    }
    "size" in {
      MinuteDomain.size must be_==(60)
    }
    "range" in {
      MinuteDomain.range(All) must be_==(List((0, 60)))
      MinuteDomain.range(Range(30, 45)) must be_==(List((30, 46)))
    }
    "rangeify" in {
      MinuteDomain.rangeify((25, 50)) must be_==(List(Range(25, 49)))
    }
  }
}