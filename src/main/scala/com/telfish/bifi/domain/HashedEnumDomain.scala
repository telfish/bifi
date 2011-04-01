package com.telfish.bifi
package domain

import collection.mutable.ArrayBuffer


class HashedEnumDomain[T](values: IndexedSeq[T]) extends EnumDomain(values) {
  val elementToIndexMap = {
    val map = new java.util.IdentityHashMap[T, Int]
    values.zipWithIndex foreach { case (v, i) => map.put(v, i) }
    map
  }

  override def indexOf(t: T): Long = elementToIndexMap.get(t)
}

