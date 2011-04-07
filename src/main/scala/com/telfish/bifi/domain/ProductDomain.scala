package com.telfish.bifi
package domain

/**
 * A product of two RangeDomains where a value can be any combination of a value from the
 * first and the second domain.
 */
case class ProductDomain[T1, T2, R1, R2](d1: RangeDomain[T1, R1], d2: RangeDomain[T2, R2]) extends RangeDomain[(T1, T2), (R1, R2)] { outer =>
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
                       e(math.min(end, range._2) % d2.size))

          d2.rangeify(r) map {
            r2 => (d1.single(v1), r2)
          }
        }
      }
    }
  }

  private def mergeTwo(first: (R1, R2), second: (R1, R2)): List[(R1, R2)] =
    (first._1 == second._1, first._2 == second._2) match {
      case (true, true) => List(first)
      case (false, true) => d1.mergeRanges(List(first._1, second._1)) map (r1 => (r1, first._2))
      case (true, false) => d2.mergeRanges(List(first._2, second._2)) map (r2 => (first._1, r2))
      case (false, false) => List(first, second)
    }

  def mergeRanges(ranges: List[(R1, R2)]): List[(R1, R2)] = ranges match {
    case Nil         => Nil
    case one::Nil    => one::Nil
    case first::rest => mergeRanges(rest) flatMap (mergeTwo(_, first))
  }
}



