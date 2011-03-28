package com.telfish.bifi

import org.specs.Specification

trait DomainMap[T, R, A] {
  def domain: RangeDomain[T, R]
  def longRangeMap: LongRangeMap[A]

  /**
   * Gets the value associated with a domain value
   */
  def get(l: T): Option[A]

  /**
   * Reports the list of gaps in the given range
   */
  def gaps: List[R]
  def overlaps: List[(R, List[A])]
}

trait DomainMapBuilder[T, R, A] {
  def domain: RangeDomain[T, R]

  def add(range: R, value: A): this.type

  def toDomainMap: DomainMap[T, R, A]
}

object DomainMapSpecs extends Specification {
  "A domain map" should {
    "map domain values to target elements" in {

    }

    "report gaps" in {

    }
    "report overlaps" in {

    }
  }
}