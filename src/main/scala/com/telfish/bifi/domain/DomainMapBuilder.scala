package com.telfish.bifi
package domain

class DomainMapBuilder[T, R, A: ClassManifest](val domain: RangeDomain[T, R]) { builder =>
  val indexMapBuilder = new LongRangeMapBuilder[A]

  def add(range: R, value: A): this.type = {
    domain.range(range) foreach { case (s, e) => indexMapBuilder.add(s, e, value) }
    this
  }
  def addSingle(single: T, value: A): this.type = {
    val i = domain.indexOf(single)
    indexMapBuilder.add(i, i + 1, value)
    this
  }

  def toDomainMap: DomainMap[T, R, A] = {
    val theMap = indexMapBuilder.toLongRangeMap

    new DomainMap[T, R, A] {
      def domain: RangeDomain[T, R] = builder.domain
      def underlyingIndexMap: LongRangeMap[A] = theMap

      def gaps: List[R] =
        theMap.gaps(domain.size) flatMap domain.rangeify

      def overlaps: List[(R, List[A])] =
        theMap.overlaps flatMap { case (s, e, values) => domain.rangeify(s, e) map ((_, values)) }

      def get(l: T): Option[A] = theMap.get(domain.indexOf(l))

      def cardinality: Int = theMap.cardinality
    }
  }
}


