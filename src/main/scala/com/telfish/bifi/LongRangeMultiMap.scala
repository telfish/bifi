package com.telfish.bifi

import domain.LongBasedDomainMap

/**
 * A LongRangeMultiMap is a map where multiple other maps can be integrated.
 * All those maps then share the start/end arrays and keep only their own value store.
 */
trait LongRangeMultiMap {
  def integrate[A <: AnyRef: ClassManifest](map: LongRangeMap[A]): LongRangeMap[A]
  def integrate[A <: AnyRef: ClassManifest](entries: Seq[Entry[A]]): LongRangeMap[A]

  def optimize()
  def dump
}

object LongRangeMultiMap {
  def create: LongRangeMultiMap = new LongRangeMultiMapImpl

  private[this] class LongRangeMultiMapImpl extends LongRangeMultiMap {
    class IntegrationEntry[A <: AnyRef](val entries: Seq[Entry[A]])(implicit val mf: ClassManifest[A]) {
      def array(n: Int): Array[AnyRef] = new Array[AnyRef](n)
    }

    var curIdx = 0
    val entriesToIntegrate = new collection.mutable.ArrayBuffer[IntegrationEntry[_ <: AnyRef]]

    case class OptimizedMap(starts: Array[Long], lengths: Array[Long], values: Array[Array[AnyRef]])
    var optimized: OptimizedMap = null

    def integrate[A <: AnyRef: ClassManifest](map: LongRangeMap[A]): LongRangeMap[A] =
      addEntry(new IntegrationEntry(map.traverseEntries.toSeq))

    def integrate[A <: AnyRef: ClassManifest](entries: Seq[Entry[A]]): LongRangeMap[A] =
      addEntry(new IntegrationEntry(entries.sortBy(_.start)))

    def optimize(): Unit = {
      // the number of maps which are now being integrated
      trait Event {
        def pos: Long
        def start: Boolean
      }
      case class NewEvent(pos: Long, start: Boolean, mapIdx: Int, valueIdx: Int) extends Event
      case class OptimizedEntryEvent(pos: Long, start: Boolean, valueIdx: Int) extends Event

      val oldEvents: Seq[Event] =
        if (optimized == null)
          Seq.empty
        else
          (0 until optimized.starts.size) map { i =>
            //List(
              OptimizedEntryEvent(optimized.starts(i), true, i)//,
              //OptimizedEntryEvent(optimized.starts(i) + optimized.lengths(i), false, i)
            //)
          }
      val mapIdxOffset =
        if (optimized == null)
          0
        else
          optimized.values.size

      val newEvents =
        for { mapIdx  <- (0 until entriesToIntegrate.size)
              iEntry    = entriesToIntegrate(mapIdx)
              valueIdx <- (0 until iEntry.entries.size)
              entry     = iEntry.entries(valueIdx)
              e        <- List(NewEvent(entry.start, true, mapIdx, valueIdx),
                               NewEvent(entry.start + entry.length, false, mapIdx, valueIdx))
            }
          yield e

      val events: Seq[Event] = oldEvents ++ newEvents

      val grouped = events.groupBy(_.pos).toSeq.sortBy(_._1).map(_._2)

      val newSize = grouped.size - 1

      val n = mapIdxOffset + entriesToIntegrate.size

      val results: Array[Array[AnyRef]] = (0 until n).map(_ => new Array[AnyRef](newSize)).toArray

      val starts = new Array[Long](newSize)
      val lengths = new Array[Long](newSize)

      val curValues = new Array[Event](1 + entriesToIntegrate.size)

      var cur = 0
      slide(grouped) { (startEvents, endEvents) =>
        val start = startEvents.head.pos
        val end   = endEvents.head.pos

        def endBeforeStart(e: Event): Int = if (e.start) 1 else 0

        startEvents.sortBy(endBeforeStart) foreach {
          case e@NewEvent(_, _, mapIdx, _) =>
            curValues(mapIdx + 1) = e
          case e@OptimizedEntryEvent(_, start, valueIdx) =>
            curValues(0) = e
        }

        starts(cur) = start
        lengths(cur) = end - start

        for (i <- (0 until (entriesToIntegrate.size + 1))) {
          curValues(i) match {
            case null =>
            case e@OptimizedEntryEvent(_, start, valueIdx) =>
              (0 until optimized.values.size) foreach { i =>
                results(i)(cur) = optimized.values(i)(valueIdx)
              }
            case e@NewEvent(_, start, mapIdx, valueIdx) =>
              results(mapIdx + mapIdxOffset)(cur) = if (start) entriesToIntegrate(mapIdx).entries(valueIdx).value else null
          }
        }

        cur += 1
      }

      optimized = OptimizedMap(starts, lengths, results)

      // release memory
      entriesToIntegrate.clear
    }

    def dump = {
      (0 until optimized.starts.size) foreach { i =>
        val start  = optimized.starts(i)
        val length = optimized.lengths(i)
        val values = optimized.values map (_(i))
        println("%5d %5d %s" format (start, length , values mkString " "))
      }
    }

    def slide[A](xs: Seq[A])(f: (A, A) => Unit): Unit =
      xs.sliding(2) foreach {
        v => f(v(0), v(1))
      }

    def addEntry[A <: AnyRef](entry: IntegrationEntry[A])(implicit mf: ClassManifest[A]): LongRangeMap[A] = {
      val idx = curIdx

      entriesToIntegrate += entry
      curIdx += 1

      if (entriesToIntegrate.size > 150) {
        println("Auto-optimizing")
        optimize()
      }

      new GenericRLELongRangeMap[A] {
        implicit def manifest: ClassManifest[A] = mf

        def starts: Array[Long] = optimized.starts
        protected def lengths: Array[Long] = optimized.lengths

        def valueAt(i: Int): A = optimized.values(idx)(i).asInstanceOf[A]

        override def get(l: Long): Option[A] = {
          val res = super.get(l)
            val filtered = res.filter(_ != null)
          filtered
        }
      }
    }
  }
}