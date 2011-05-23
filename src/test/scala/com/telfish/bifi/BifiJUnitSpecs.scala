package com.telfish.bifi

import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

@RunWith(classOf[JUnitSuiteRunner])
class BifiJUnitSpecs extends JUnit4(
  LongRangeMapSpecs, LongRangeMultiMapSpecs,
  domain.DomainSpecs, domain.ConcreteDomainSpecs, domain.DomainMapSpecs
)
