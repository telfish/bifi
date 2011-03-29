package com.telfish.bifi

trait DomainMap[T, R, A] {
  def domain: RangeDomain[T, R]
  def underlyingIndexMap: LongRangeMap[A]

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
        theMap.gaps(domain.size) map domain.rangeify

      def overlaps: List[(R, List[A])] =
        theMap.overlaps map { case (s, e, values) => (domain.rangeify(s, e), values)}

      def get(l: T): Option[A] = theMap.get(domain.indexOf(l))
    }
  }
}
