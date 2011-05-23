package com.telfish.bifi

import com.telfish.bifi.LongRangeMultiMapOptimizer.EventMapping
import org.specs.{ScalaCheck, Specification}
import org.scalacheck.Prop
import Prop._

object LongRangeMultiMapSpecs extends Specification with ScalaCheck {
  "Event mapping" should {
    "map values properly" in {
      "mapIdx automatically" verifies { (a: Byte) =>
        val event = EventMapping.createEvent(a, false, 0)
        EventMapping.mapIdx(event) == a
      }

      "mapIdx" in {
        val event = EventMapping.createEvent(18, false, 0)
        EventMapping.mapIdx(event) must be_==(18)

        "== -1" in {
          val event = EventMapping.createEvent(-1, false, 0)
          EventMapping.mapIdx(event) must be_==(-1)
        }
      }

      "valueIdx automatically" verifies { (a: Short) =>
        val event = EventMapping.createEvent(1, false, a)
        (a > 0) ==> (EventMapping.valueIdx(event) == a)
      }

      "start automatically" verifies { (a: Byte, start: Boolean, c: Short) =>
        val event = EventMapping.createEvent(a, start, c)
        (c > 0) ==> (EventMapping.start(event) == start)
      }

      "complete" in {
        val event = EventMapping.createEvent(1, false, 0)

        EventMapping.mapIdx(event) must be_==(1)
        EventMapping.start(event) must be_==(false)
        EventMapping.valueIdx(event) must be_==(0)
      }
    }
  }
}