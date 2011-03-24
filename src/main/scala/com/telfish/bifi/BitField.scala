package com.telfish.bifi

/**
 * A bit field is an immutable data structure to represent a
 * field of bits of Long.MAX_VALUE size. Starting from a field
 * where every value is `0`, it is currently only possible to
 * set single or consecutive ranges of bits to `1` or to calculate
 * the bitwise OR from to BitFields.
 */
trait BitField extends (Long => Boolean) {
  /**
   * Set the bit at position `index`.
   */
  def set(index: Long): BitField

  /**
   * Set `count` consecutive bits starting from position `index`.
   */
  def setN(index: Long, count: Long): BitField

  /**
   * Check if bit at position `index` is set.
   */
  def isSet(index: Long): Boolean

  /**
   * An alias for [[apply]]
   */
  def apply(index: Long): Boolean = isSet(index)

  /**
   * Get the number of set bits in this field.
   */
  def cardinality: Long

  /**
   * Calculate the bitwise OR of `this` and `other` field.
   */
  def |(other: BitField): BitField

  /**
   * Check if `this` field overlaps with `other`, i.e. if the
   * intersection has bits set.
   */
  def overlaps(other: BitField): Boolean =
    this.cardinality + other.cardinality == (this | other).cardinality
}

object BitField {
  def empty: BitField = HashBitField.empty
  def apply(bit0: Long, otherBits: Long*): BitField = apply(bit0 +: otherBits)
  def apply(setBits: Traversable[Long]): BitField = setBits.foldLeft(empty)(_ set _)
}