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

  /**
   * Gets the value associated with the index corresponding to a domain value
   */
  def getByIndex(idx: Long): Option[A]

  /**
   * Reports the list of gaps in the given range
   */
  def gaps: List[R]
  def overlaps: List[(R, List[A])]

  def normalize[B: ClassManifest](merge: List[A] => B): DomainMap[T, R, B]

  def |[B: ClassManifest](other: DomainMap[T, R, B]): RangeMap[R, (Option[A], Option[B])]

  def map[B: ClassManifest](f: A => B): DomainMap[T, R, B]
}


