package com.telfish.bifi
package domain

class LongBasedDomainMap[T, R, A: ClassManifest](val domain: RangeDomain[T, R], protected[bifi] val theMap: LongRangeMap[A]) extends DomainMap[T, R, A] {
  def underlyingIndexMap: LongRangeMap[A] = theMap

  def gaps: List[R] =
    theMap.gaps(domain.size) flatMap domain.rangeify

  def overlaps: List[(R, List[A])] =
    theMap.overlaps flatMap { case (s, e, values) => domain.rangeify(s, e) map ((_, values)) }

  def get(l: T): Option[A] = getByIndex(domain.indexOf(l))
  def getByIndex(idx: Long): Option[A] = theMap.get(idx)

  def cardinality: Int = theMap.cardinality

  def |[B: ClassManifest](other: DomainMap[T, R, B]): RangeMap[R, (Option[A], Option[B])] = other match {
    case l: LongBasedDomainMap[_, _, _] =>
        new DomainRangeMap(theMap | l.theMap) flatMapRange { case (s, e) => domain.rangeify(s, e) }
    case _ => throw new UnsupportedOperationException("| not supported with DomainMaps other than LongBasedDomainMaps")
  }

  def ++(other: DomainMap[T, R, A]): DomainMap[T, R, A] = other match {
    case l: LongBasedDomainMap[_, _, _] =>
      new LongBasedDomainMap(domain, theMap ++ l.theMap)
    case _ => throw new UnsupportedOperationException("++ not supported with DomainMaps other than LongBasedDomainMaps")
  }

  def normalize[B: ClassManifest](merge: (List[A]) => B): DomainMap[T, R, B] = {
    val normalized = theMap.normalize(merge) map {
      case (s, e, v) => Entry[B](s, e - s, v)
    }
    new LongBasedDomainMap(domain, RLELongRangeMap.fromSortedEntries[B](normalized.toIterable))
  }

  class DomainRangeMap[A](indexMap: Traversable[(Long, Long, A)]) extends RangeMap[(Long, Long), A] {
    def foreach[U](f: (((Long, Long), A)) => U) {
      indexMap foreach {
        case (s, e, a) => f(((s, e), a))
      }
    }
  }

  def map[B: ClassManifest](f: (A) => B): DomainMap[T, R, B] = new LongBasedDomainMap(domain, theMap.map(f))

  def integrateInto(multiMap: LongRangeMultiMap)(implicit ev: A <:< AnyRef): DomainMap[T, R, A] =
    new LongBasedDomainMap[T, R, A](domain, multiMap.integrate(theMap.asInstanceOf[LongRangeMap[AnyRef]]).asInstanceOf[LongRangeMap[A]])
}

class DomainMapBuilder[T, R, A: ClassManifest](val domain: RangeDomain[T, R]) { builder =>
  val indexMapBuilder = new LongRangeMapBuilder[A]

  def add(range: R, value: A): this.type = {
    domain.range(range) foreach { r => indexMapBuilder.add(r._1, r._2, value) }
    this
  }
  def addSingle(single: T, value: A): this.type = {
    val i = domain.indexOf(single)
    indexMapBuilder.add(i, i + 1, value)
    this
  }

  def toDomainMap: DomainMap[T, R, A] = {
    val theMap = indexMapBuilder.toLongRangeMap

    new LongBasedDomainMap[T, R, A](domain, theMap)
  }
}


