package com.telfish.bifi

import org.specs.Specification
import org.scalacheck._
import org.specs.matcher.ScalaCheckMatchers

/**
 * Extends the normal ScalaCheck verifiers with something which accepts a function => Prop as well
 */
trait BetterScalaCheckVerifies {
  self: ScalaCheckMatchers with Specification =>

  implicit def toVerifies2(e: String): VerifiableExpectation2 = VerifiableExpectation2(e: String)

  case class VerifiableExpectation2(e: String) {
    def verifies[A1: Arbitrary : Shrink, A2: Arbitrary : Shrink, P <% Prop](p: (A1, A2) => P) =
      forExample(e) in {
        p.must(pass(Prop.forAll(_: (A1, A2) => P) ) )
      }
  }
}
