package com.telfish.bifi
package domain

trait DomainMap[T, R, A] {
  def domain: RangeDomain[T, R]
  def underlyingIndexMap: LongRangeMap[A]

  def cardinality: Int

  /**
   * Gets the value associated with a domain value
   */
  def get(l: T): Option[A]

  type Token = Int
  def getTokenByIndex(l: Long): Token
  def isDefinedAtToken(token: Token): Boolean
  def getByToken(token: Token): A

  /**
   * Reports the list of gaps in the given range
   */
  def gaps: List[R]
  def overlaps: List[(R, List[A])]

  def normalize[B: ClassManifest](merge: List[A] => B): DomainMap[T, R, B]
  def normalizeWithRange[B: ClassManifest](merge: (() => List[R], List[A]) => B): DomainMap[T, R, B]

  def |[B: ClassManifest](other: DomainMap[T, R, B]): RangeMap[R, (Option[A], Option[B])]

  def ++(other: DomainMap[T, R, A]): DomainMap[T, R, A]

  def map[B: ClassManifest](f: A => B): DomainMap[T, R, B]

  def integrateInto(multiMap: LongRangeMultiMap)(implicit ev: A <:< AnyRef): DomainMap[T, R, A]
}

object DomainMap {
  def empty[T, R, A](domain: RangeDomain[T, R]) = new LongBasedDomainMap(domain, LongRangeMap.empty[A])
}


