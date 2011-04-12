package com.telfish.bifi
package domain

trait RangeMap[R, B] extends Traversable[(R, B)] { outer =>
  def flatMapRange[R2](func: R => Traversable[R2]): RangeMap[R2, B] =
    new RangeMap[R2, B] {
      def foreach[U](f: ((R2, B)) => U) {
        outer.foreach {
          case (r, b) => func(r) foreach { r2 =>
            f((r2, b))
          }
        }
      }

      override def filterValues(pred: (B) => Boolean): RangeMap[R2, B] =
        // reverse application of filter and range flatMap, so that func is evaluated as late as possible
        outer.filterValues(pred).flatMapRange(func)
    }

  def filterValues(pred: B => Boolean): RangeMap[R, B] =
    new RangeMap[R, B] {
      def foreach[U](f: ((R, B)) => U) {
        outer.foreach {
          case ((r, b)) => if (pred(b)) f((r, b))
        }
      }
    }
}
