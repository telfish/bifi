package com.telfish.bifi

import collection.immutable.HashSet

class HashBitField protected (protected val map: HashSet[Long]) extends BitField {
  private[this] def withMap(map: HashSet[Long]) = new HashBitField(map)

  def set(index: Long): BitField = withMap(map + index)

  def setN(index: Long, count: Long): BitField = withMap((index to (index + count - 1)).foldLeft(map)(_ + _))

  def cardinality: Long = map.size

  def isSet(index: Long): Boolean = map(index)

  def |(other: BitField): BitField = other match {
    case that: HashBitField => withMap(this.map union that.map)
    case _ => throw new UnsupportedOperationException("| is only supported with other HashBitFields")
  }
}

object HashBitField {
  def empty = new HashBitField(HashSet.empty)
}