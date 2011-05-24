package com.telfish.bifi

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

  class IntegrationEntry(val entries: Seq[Entry[_]])
  case class OptimizedMap(starts: Array[Long], lengths: Array[Long], values: Array[Array[AnyRef]])

  private[this] class LongRangeMultiMapImpl extends LongRangeMultiMap {
    var curIdx = 0
    val entriesToIntegrate = new collection.mutable.ArrayBuffer[IntegrationEntry]
    var optimized: OptimizedMap = OptimizedMap(Array.empty, Array.empty, Array.empty)

    def integrate[A <: AnyRef: ClassManifest](map: LongRangeMap[A]): LongRangeMap[A] =
      addEntry(new IntegrationEntry(map.traverseEntries.toSeq))

    def integrate[A <: AnyRef: ClassManifest](entries: Seq[Entry[A]]): LongRangeMap[A] =
      addEntry(new IntegrationEntry(entries.sortBy(_.start)))

    def optimize(): Unit = {
      optimized = LongRangeMultiMapOptimizer.optimize(optimized, entriesToIntegrate)
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

    final def slide[A <: AnyRef](xs: Seq[A])(f: (A, A) => Unit): Unit = {
      var last: A = null.asInstanceOf[A]
      var initialized = false
      xs.foreach { a =>
        if (initialized) {
          f(last, a)
        } else
          initialized = true

        last = a
      }
    }
    val createdAt = System.currentTimeMillis
    def addEntry[A](entry: IntegrationEntry)(implicit mf: ClassManifest[A]): LongRangeMap[A] = {
      val idx = curIdx

      entriesToIntegrate += entry
      curIdx += 1

      if (entriesToIntegrate.size > System.getProperty("bifi.autooptimize.size", "100").toInt) {
        println("Auto-optimizing")
        val number = entriesToIntegrate.size
        val start = System.currentTimeMillis
        optimize()
        val dur = System.currentTimeMillis - start
        println("Auto-optimizing of %d element lasted %d: %.2f" format (number, dur, dur.toDouble / number))
        val wholeDur = System.currentTimeMillis - createdAt
        println("Processed %d elements in %d ms (%.2f ms / element)" format (optimized.values.size, wholeDur, wholeDur.toDouble / optimized.values.size))
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

object LongRangeMultiMapOptimizer {
  import LongRangeMultiMap._
  def optimize(optimized: OptimizedMap, entriesToIntegrate: IndexedSeq[IntegrationEntry]): OptimizedMap = {
    val mapIdxOffset = optimized.values.size

    val events: Seq[Event] = Event.allEvents//null//oldEvents ++ newEvents

    val grouped = events.groupBy(Event.pos).toSeq.sortBy(_._1).map(_._2)

    val newSize = grouped.size - 1

    val n = mapIdxOffset + entriesToIntegrate.size

    import collection.mutable.ArrayBuffer
    val starts = new ArrayBuffer[Long](newSize)
    val lengths = new ArrayBuffer[Long](newSize)

    val results: Array[ArrayBuffer[AnyRef]] = (0 until n).map(_ => new ArrayBuffer[AnyRef](newSize)).toArray

    // the running state, collects information which of the results entries is at which state
    val curValues = Array.fill[Event](1 + entriesToIntegrate.size)(Event.UNSET)

    var cur = -1
    slide(grouped) { (startEvents, endEvents) =>
      val start = Event.pos(startEvents.head)
      val end   = Event.pos(endEvents.head)

      def endBeforeStart(e: Event): Int = if (Event.start(e)) 1 else 0

      startEvents.sortBy(endBeforeStart) foreach (e => curValues(Event.indexFor(e)) = e)
      /*{
        case e@NewEvent(_, _, mapIdx, _) =>
          curValues(mapIdx + 1) = e
        case e@OptimizedEntryEvent(_, valueIdx) =>
          curValues(0) = e
      }*/

      var created = false
      def set(mapIdx: Int, value: AnyRef) {
        if (value != null) {
          if (!created) {
            created = true

            starts  += start
            lengths += (end - start)
            results.foreach(_ += null)

            cur += 1
          }

          results(mapIdx)(cur) = value
        }
      }

      for (i <- (0 until (entriesToIntegrate.size + 1))) {
        curValues(i) match {
          case Event.UNSET =>
          case e: Event => Event.handle(e)(set)
          /*
          case e@OptimizedEntryEvent(_, valueIdx) =>
            (0 until optimized.values.size) foreach { i =>
              set(i, optimized.values(i)(valueIdx))
            }
          case e@NewEvent(_, start, mapIdx, valueIdx) =>
            set(mapIdx + mapIdxOffset, if (start) entriesToIntegrate(mapIdx).entries(valueIdx).value else null)
            */
        }
      }
    }

    OptimizedMap(
      starts.toArray,
      lengths.toArray,
      results.map(_.toArray))
  }

  // we define a mapping where we can identify each possible event
  // by a single 32-bit integer value
  // This integer is defined as follows:
  //  - 8bits : mapIdx or -1 if valueIdx goes into previously OptimizedMap
  //  - 1 bit : start
  //  - 23bits: valueIdx
  type Event = Int
  val mapMask = 256 << 24
  val endMask = 1 << 23
  val valueMask = 0xffffffff >> 9

  def createEvent(mapIndex: Int, end: Boolean, valueIndex: Int): Event = {
    val mapBits = mapIndex.toByte.toInt << 24
    val endBits = (if (end) 1 else 0) << 23
    val valueBits = valueIndex & valueMask
    mapBits | endBits | valueBits
  }

  def mapIdx(e: Event): Int = (e & mapMask) >> 24
  def start(e: Event): Boolean = (e & endMask) == 0
  def valueIdx(e: Event): Int = e & valueMask

  class EventMapping(optimized: OptimizedMap, entriesToIntegrate: IndexedSeq[IntegrationEntry]) {
    object Old {
      def pos(e: Event): Long = optimized.starts(e)
      def start(e: Event): Boolean = true
      def indexFor(e: Event): Int = 0
      def handle(e: Event)(set: (Int, AnyRef) => Unit) = {
        (0 until optimized.values.size) foreach { i =>
          set(i, optimized.values(i)(e))
        }
      }

      def allEvents = (0 until optimized.starts.size) map { i => createEvent(-1, false, i) }
    }

    object New {
      val startOffsets = entriesToIntegrate.scanLeft(0)(_ + _.entries.size)

      def entry(e: Event): Entry[_ <: AnyRef] =
        entriesToIntegrate(mapIdx(e)).entries(valueIdx(e))

      def pos(e: Event): Long = {
        val en = entry(e)
        if (start(e))
          en.start
        else
          en.start + en.length
      }

      def indexFor(e: Event): Int = 1 + mapIdx(e)
      def handle(e: Event)(set: (Int, AnyRef) => Unit): Unit =
        set(mapIdx(e) + mapIdxOffset, if (start(e)) entry(e).value else null)

      def allEvents =
        for { mapIdx  <- (0 until entriesToIntegrate.size)
          iEntry    = entriesToIntegrate(mapIdx)
          valueIdx <- (0 until iEntry.entries.size)
          end      <- List(false, true)
        }
          yield createEvent(mapIdx, end, valueIdx)
    }

    def isOld(e: Event): Boolean = mapIdx(e) == -1;

    def pos(e: Event): Long = if (isOld(e)) Old.pos(e) else New.pos(e)

    def indexFor(e: Event): Int = if (isOld(e)) Old.indexFor(e) else New.indexFor(e)
    def handle(e: Event)(f: (Int, AnyRef) => Unit) = if (isOld(e)) Old.handle(e)(f) else New.handle(e)(f)

    def allEvents = Old.allEvents ++ New.allEvents

    val UNSET: Event = -1
  }

}