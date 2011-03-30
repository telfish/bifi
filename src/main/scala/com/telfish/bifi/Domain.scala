package com.telfish.bifi

/**
 * A domain is a mapping from all elements of type T of a domain of finite, discrete values
 * to a characteristic Long value designating this element (and back).
 */
trait Domain[T] { outer =>
  type Value = T

  def size: Long
  def indexOf(t: T): Long
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

trait RangeDomain[T, R] extends Domain[T] {
  type RangeT = R

  def range(expr: R): List[(Long, Long)]

  def rangeify(range: (Long, Long)): R

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

  def range(expr: (R1, R2)): List[(Long, Long)] =
    for { (start1, end1) <- d1.range(expr._1)
          i1             <- start1 until end1
          (start2, end2) <- d2.range(expr._2)
        }
      yield (i1 * d2.size + start2, i1 * d2.size + end2)

  def rangeify(range: (Long, Long)): (R1, R2) = null
}

object Domain {
  //implicit def tupled3Domain[T1, T2, T3](d1: Domain[T1], d2: Domain[T2], d3: Domain[T3]): Domain[(T1, T2, T3)] =
//    tupled2Domain(d1, tupled2Domain(d2, d3)).map({ case (a, (b, c)) => (a, b, c) }, { case (a, b, c) => (a, (b, c)) })
    /* faster direct version:

    new Domain[(T1, T2, T3)] {
      def elementAt(pos: Long): (T1, T2, T3) = {
        val i1 = pos / d3.size / d2.size
        val i2 = pos / d3.size % d2.size
        val i3 = pos % d3.size

        (d1.elementAt(i1), d2.elementAt(i2), d3.elementAt(i3))
      }

      def indexOf(t: (T1, T2, T3)): Long = (d1.indexOf(t._1) * d2.size + d2.indexOf(t._2)) * d3.size + d3.indexOf(t._3)

      def size: Long = d1.size * d2.size * d3.size
    }*/
}