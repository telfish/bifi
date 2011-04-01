package com.telfish.bifi
package domain

/**
 * A domain is a mapping from all elements of a domain of finite, discrete values of type T
 * to a characteristic Long value between 0 and `size` designating this element (and back).
 */
trait Domain[T] { outer =>
  type Value = T

  /**
   * The number of all values in this domain
   */
  def size: Long

  /**
   * The characteristic Long value of an element
   */
  def indexOf(t: T): Long

  /**
   * The element for a characteristic value.
   */
  def elementAt(pos: Long): T

  def values: Seq[T] = new IndexedSeq[T] {
    def apply(idx: Int): T = elementAt(idx.toLong)
    def length: Int = Domain.this.size.toInt
  }

  def map[U](f: T => U, fInv: U => T): Domain[U] = new Domain[U] {
    def elementAt(pos: Long): U = f(outer.elementAt(pos))
    def indexOf(t: U): Long = outer.indexOf(fInv(t))
    def size: Long = Domain.this.size
  }
}

/**
 * A domain where a consecutive range of values of type T corresponds to
 * a consecutive range of characteristic Long values.
 */



