package com.telfish.bifi

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

  /**
   * The range of values between start and end inclusive.
   */
  def indexRange(start: T, end: T): (Long, Long) =
    (indexOf(start), indexOf(end) + 1)

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

trait RangeDomain[T, R] extends Domain[T] {
  type RangeT = R
  type MapT[A] = DomainMap[T, R, A]

  def mapBuilder[A: ClassManifest]: DomainMapBuilder[T, R, A] = new DomainMapBuilder[T, R, A](this)

  def range(expr: R): List[(Long, Long)]

  def rangeify(range: (Long, Long)): List[R]

  def valuesInRange(expr: R): Traversable[T] = range(expr).flatMap {
    case (start, end) => (start until end) map elementAt
  }
  def single(t: T): R = rangeify(indexRange(t, t)).head

  def ×[T2, R2](other: RangeDomain[T2, R2]): DomainProduct[T, T2, R, R2] = new DomainProduct(this, other)
}

trait RangeExpr[+T]
case class Range[T](from: T, to: T) extends RangeExpr[T]
case class Single[T](point: T) extends RangeExpr[T]
case object All extends RangeExpr[Nothing]

object RangeExpr {
  def range[T](from: T, to: T): RangeExpr[T] = Range(from, to)
  def single[T](t: T): RangeExpr[T] = Single(t)
  def all = All

  def rangeByExpr[T](expr: RangeExpr[T], domain: Domain[T]): List[(Long, Long)] = List(expr match {
    case Single(t) =>
      val index = domain.indexOf(t)
      (index, index + 1)
    case Range(from, to) =>
      (domain.indexOf(from), domain.indexOf(to) + 1)
    case All =>
      (0L, domain.size)
  })

  def exprByRange[T](range: (Long, Long), domain: Domain[T]): RangeExpr[T] = range match {
    case (a, b) if a + 1 == b       => Single(domain.elementAt(a))
    case (0, x) if x == domain.size => All
    case (a, b) if a < b            => Range(domain.elementAt(a), domain.elementAt(b - 1))
    case (a, b)                     => throw new IllegalStateException("invalid range: "+a+" - "+b)
  }
}

case class DomainProduct[T1, T2, R1, R2](d1: RangeDomain[T1, R1], d2: RangeDomain[T2, R2]) extends RangeDomain[(T1, T2), (R1, R2)] { outer =>
  def elementAt(pos: Long): (T1, T2) = {
    val i1 = pos / d2.size
    val i2 = pos % d2.size

    (d1.elementAt(i1), d2.elementAt(i2))
  }

  def indexOf(t: (T1, T2)): Long = d1.indexOf(t._1) * d2.size + d2.indexOf(t._2)

  def size: Long = d1.size * d2.size

  def range(expr: (R1, R2)): List[(Long, Long)] = {
    def product(start1: Long, end1: Long, start2: Long, end2: Long): Traversable[(Long, Long)] =
      if (start2 == 0 && end2 == d2.size)
        List((start1 * d2.size, end1 * d2.size))
      else
        (start1 until end1) map (i1 => (i1 * d2.size + start2, i1 * d2.size + end2))

    for { (start1, end1) <- d1.range(expr._1)
          (start2, end2) <- d2.range(expr._2)
          p              <- product(start1, end1, start2, end2)
        }
      yield p
  }

  def rangeify(range: (Long, Long)): List[(R1, R2)] = {
    val (start1, start2) = elementAt(range._1)
    val (end1  , end2)   = elementAt(range._2 - 1)

    val d1Ranges = d1.rangeify(d1.indexRange(start1, end1))
    val d2Range@(startIndex2, endIndex2) = d2.indexRange(start2, end2)

    if (start1 == end1 || (startIndex2 == 0 && endIndex2 == d2.size)) {
      val List(e2) = d2.rangeify(d2Range)

      d1Ranges map { e1 => (e1, e2) }
    } else {
      d1Ranges flatMap { r1 =>
        d1.valuesInRange(r1) flatMap { v1 =>
          val start = d1.indexOf(v1) * d2.size
          val end   = start + d2.size
          def e(end: Long): Long = if (end == 0) d2.size else end
          val r     = (math.max(start, range._1) % d2.size,
                       e(math.min(end  , range._2) % d2.size))

          d2.rangeify(r) map {
            r2 => (d1.single(v1), r2)
          }
        }
      }
    }
  }
}
