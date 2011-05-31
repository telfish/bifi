package com.telfish.bifi

/**
 * A LongRangeMultiMap is a map where multiple other maps can be integrated.
 * All those maps then share the start/end arrays and only a copy of their value store is kept.
 * A LongRangeMultiMap is not necessarily reentrant.
 */
trait LongRangeMultiMap {
  def integrate[A <: AnyRef: ClassManifest](map: LongRangeMap[A]): LongRangeMap[A]
  def integrate[A <: AnyRef: ClassManifest](entries: Seq[Entry[A]]): LongRangeMap[A]

  /**
   * Optimizes this MultiMap and returns an 'immutable' MultiMap where new entries
   * won't be integrated but instead are just returned.
   */
  def optimize(): LongRangeMultiMap
  def dump: String
}

object LongRangeMultiMap {
  def create(tag: String = "<unnamed>"): LongRangeMultiMap = new LongRangeMultiMapImpl(tag)

  trait IntegrationEntry {
    def start(i: Int): Long
    def length(i: Int): Long
    def value(i: Int): AnyRef
    def size: Int
  }
  case class SimpleIntegrationEntry(entries: Seq[Entry[_ <: AnyRef]]) extends IntegrationEntry {
    def start(i: Int): Long = entries(i).start
    def length(i: Int): Long = entries(i).length
    def value(i: Int): AnyRef = entries(i).value

    def size: Int = entries.size
  }
  case class OptimizedMap(starts: Array[Long], lengths: Array[Long], values: Array[Array[AnyRef]])

  private[this] class LongRangeMultiMapImpl(val tag: String) extends LongRangeMultiMap {
    var curIdx = 0
    val entriesToIntegrate = new collection.mutable.ArrayBuffer[IntegrationEntry]
    var optimized: OptimizedMap = OptimizedMap(Array.empty, Array.empty, Array.empty)

    def integrate[A <: AnyRef: ClassManifest](map: LongRangeMap[A]): LongRangeMap[A] =
      if (map.cardinality > 0)
        addEntry(new SimpleIntegrationEntry(map.traverseEntries.toSeq))
      else
        map

    def integrate[A <: AnyRef: ClassManifest](entries: Seq[Entry[A]]): LongRangeMap[A] =
      if (entries.size > 0)
        addEntry(new SimpleIntegrationEntry(entries.sortBy(_.start)))
      else
        throw new RuntimeException("Tried to integrate an empty set of entries")


    def optimize(): LongRangeMultiMap = {
      optimized = LongRangeMultiMapOptimizer.optimize(optimized, entriesToIntegrate)
      entriesToIntegrate.clear
      val that = this

      new LongRangeMultiMap {
        def integrate[A <: AnyRef : ClassManifest](map: LongRangeMap[A]): LongRangeMap[A] = map
        def integrate[A <: AnyRef : ClassManifest](entries: Seq[Entry[A]]): LongRangeMap[A] =
          RLELongRangeMap.fromSortedEntries(entries.sortBy(_.start))

        def optimize(): LongRangeMultiMap = this
        def dump: String = that.dump
      }
    }

    def dump: String = {
      val res =
        (0 until optimized.starts.size) map { i =>
          val start  = optimized.starts(i)
          val length = optimized.lengths(i)
          val values = optimized.values map (_(i))
          "%5d %5d %s" format (start, length , values mkString " ")
        } mkString "\n"

      println(res)
      res
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
    val Event = new EventMapping(optimized, entriesToIntegrate)

    val mapIdxOffset = optimized.values.size

    val grouped = Event.allEvents.groupBy(Event.pos).toSeq.sortBy(_._1).map(_._2)

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

      def endBeforeStart(e: Event): Int = if (EventMapping.start(e)) 1 else 0

      startEvents.sortBy(endBeforeStart) foreach (e => curValues(Event.indexFor(e)) = e)

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
        }
      }
    }

    OptimizedMap(
      starts.toArray,
      lengths.toArray,
      results.map(_.toArray))
  }

  type Event = Int
  class EventMapping(optimized: OptimizedMap, entriesToIntegrate: IndexedSeq[IntegrationEntry]) {
    import EventMapping._

    lazy val mapIdxOffset = optimized.values.size

    object Old {
      def pos(e: Event): Long = optimized.starts(valueIdx(e))
      def start(e: Event): Boolean = true
      def indexFor(e: Event): Int = 0
      def handle(e: Event)(set: (Int, AnyRef) => Unit) = {
        (0 until optimized.values.size) foreach { i =>
          set(i, optimized.values(i)(valueIdx(e)))
        }
      }

      def allEvents = (0 until optimized.starts.size) map { i => createEvent(-1, start = true, i) }
    }

    object New {
      def pos(e: Event): Long = {
        val iEntry = entriesToIntegrate(mapIdx(e))
        val startV = iEntry.start(valueIdx(e))
        if (start(e))
          startV
        else
          startV + iEntry.length(valueIdx(e))
      }

      def indexFor(e: Event): Int = 1 + mapIdx(e)
      def handle(e: Event)(set: (Int, AnyRef) => Unit): Unit =
        set(mapIdx(e) + mapIdxOffset, if (start(e)) entriesToIntegrate(mapIdx(e)).value(valueIdx(e)) else null)

      def allEvents =
        for { mapIdx  <- (0 until entriesToIntegrate.size)
          iEntry    = entriesToIntegrate(mapIdx)
          valueIdx <- (0 until iEntry.size)
          start      <- List(false, true)
        }
          yield createEvent(mapIdx.toByte, start = start, valueIdx)
    }

    def isOld(e: Event): Boolean = mapIdx(e) == -1;

    def pos(e: Event): Long = if (isOld(e)) Old.pos(e) else New.pos(e)

    def indexFor(e: Event): Int = if (isOld(e)) Old.indexFor(e) else New.indexFor(e)
    def handle(e: Event)(f: (Int, AnyRef) => Unit) = if (isOld(e)) Old.handle(e)(f) else New.handle(e)(f)

    def allEvents = Old.allEvents ++ New.allEvents

    val UNSET: Event = -1
  }
  object EventMapping {
    // we define a mapping where we can identify each possible event
    // by a single 32-bit integer value
    // This integer is defined as follows:
    //  - 8bits : mapIdx or -1 if valueIdx goes into previously OptimizedMap
    //  - 1 bit : start
    //  - 23bits: valueIdx
    val mapMask   = 255 << 24
    val startMask = 1 << 23
    val valueMask = startMask - 1

    def createEvent(mapIndex: Byte, start: Boolean, valueIndex: Int): Event = {
      val mapBits = (mapIndex << 24) & mapMask
      val startBits = (if (start) 1 else 0) << 23
      val valueBits = valueIndex & valueMask

      mapBits | startBits | valueBits
    }

    def mapIdx(e: Event): Byte = ((e & mapMask) >> 24).toByte
    def start(e: Event): Boolean = (e & startMask) != 0
    def valueIdx(e: Event): Int = e & valueMask
  }

  // TODO: move to commons, we need this in LongRangeMap.normalize as well
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
}