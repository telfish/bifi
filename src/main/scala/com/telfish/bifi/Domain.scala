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

  def range(start: T, end: T): List[(Long, Long)] = List((indexOf(start), indexOf(end) + 1))

  def values: Seq[T] = new IndexedSeq[T] {
    def apply(idx: Int): T = elementAt(idx.toLong)
    def length: Int = Domain.this.size.toInt
  }

  def map[U](f: T => U, fInv: U => T): Domain[U] = new Domain[U] {
    def elementAt(pos: Long): U = f(outer.elementAt(pos))
    def indexOf(t: U): Long = outer.indexOf(fInv(t))
    def size: Long = Domain.this.size
    override def range(start: U, end: U) = outer.range(fInv(start), fInv(end))
  }
}

object Domain {
  implicit def tupled2Domain[T1, T2](implicit d1: Domain[T1], d2: Domain[T2]): Domain[(T1, T2)] =
    new Domain[(T1, T2)] {
      def elementAt(pos: Long): (T1, T2) = {
        val i1 = pos / d2.size
        val i2 = pos % d2.size

        (d1.elementAt(i1), d2.elementAt(i2))
      }

      def indexOf(t: (T1, T2)): Long = d1.indexOf(t._1) * d2.size + d2.indexOf(t._2)

      def size: Long = d1.size * d2.size

      override def range(start: (T1, T2), end: (T1, T2)): List[(Long, Long)] =
        (d1.indexOf(start._1) to d1.indexOf(end._1))
          .map(d1.elementAt)
          .flatMap(t1 => d2.range(start._2, end._2) map { case (s, e) => (indexOf((t1, d2.elementAt(s))), indexOf((t1, d2.elementAt(e)))) } )
          .toList
    }

  implicit def tupled3Domain[T1, T2, T3](d1: Domain[T1], d2: Domain[T2], d3: Domain[T3]): Domain[(T1, T2, T3)] =
    tupled2Domain(d1, tupled2Domain(d2, d3)).map({ case (a, (b, c)) => (a, b, c) }, { case (a, b, c) => (a, (b, c)) })
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