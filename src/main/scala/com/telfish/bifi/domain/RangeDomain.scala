package com.telfish.bifi
package domain

/**
 * A domain is a mapping from all elements of a domain of finite, discrete values of type T
 * to a characteristic Long value between 0 and `size` designating this element (and back).
 */
trait RangeDomain[T, R] extends Domain[T] {
  type RangeT = R
  type MapT[A] = DomainMap[T, R, A]

  def mapBuilder[A: ClassManifest]: DomainMapBuilder[T, R, A] = new DomainMapBuilder[T, R, A](this)

  def range(expr: R): List[(Long, Long)]

  def rangeify(range: (Long, Long)): List[R]

  def mergeRanges(ranges: List[R]): List[R]

  def valuesInRange(expr: R): Traversable[T] = range(expr).flatMap {
    case (start, end) => (start until end) map elementAt
  }

  /**
   * The range of values between start and end inclusive.
   */
  def indexRange(start: T, end: T): (Long, Long) =
    (indexOf(start), indexOf(end) + 1)

  def single(t: T): R = rangeify(indexRange(t, t)).head

  def ×[T2, R2](other: RangeDomain[T2, R2]): ProductDomain[T, T2, R, R2] = new ProductDomain(this, other)
}
