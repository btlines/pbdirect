package pbdirect

import org.scalameter.Gen
import org.scalameter.api.Bench
import cats.instances.option._
import cats.instances.list._

object PBReaderBenchmark extends Bench.LocalTime {
  case class Message[V](v: V)
  
  /////////////////////////////////////////////////////////////////////////////
  // The following microbenchmarking tests a few scenarios:
  // * Reading a case class with many fields
  // * Repeatedly reading small objects
  // * Reading large collections of small objects
  /////////////////////////////////////////////////////////////////////////////

  def make4Tuple(payload: Array[Byte]): Tuple4[Array[Byte], Array[Byte], Array[Byte], Array[Byte]] = {
    (
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone
    )
  }

  def make8Tuple(payload: Array[Byte]): Tuple8[Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte]] = {
    (
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone
    )
  }

  def make16Tuple(payload: Array[Byte]): Tuple16[Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte]] = {
    (
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone,
    payload.clone
    )
  }


  private val sizes = Gen.range("size")(100000, 1000000, 100000)
  private def makePayload(size: Int) = Array.fill(size)(1.toByte)

  private val fourWide = for (size <- sizes) yield make4Tuple(makePayload(size)).toPB
  private val eightWide = for (size <- sizes) yield make8Tuple(makePayload(size)).toPB
  private val sixteenWide = for (size <- sizes) yield make16Tuple(makePayload(size)).toPB

  performance of "4-Wide Tuple" in {
    measure method "pbTo" in {
      using(fourWide) in {
        d => d.pbTo[Tuple4[Array[Byte], Array[Byte], Array[Byte], Array[Byte]]]
      }
    }
  }

  performance of "8-Wide Tuple" in {
    measure method "pbTo" in {
      using(eightWide) in {
        d => d.pbTo[Tuple8[Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte]]]
      }
    }
  }

  performance of "16-Wide Tuple" in {
    measure method "pbTo" in {
      using(sixteenWide) in {
        d => d.pbTo[Tuple16[Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte], Array[Byte]]]
      }
    }
  }

  private val counts = Gen.range("count")(10000, 100000, 10000)
  private val smallStrings = for (count <- counts) yield Array.fill(count)(Message("Hello!").toPB)

  performance of "many small objects" in {
    measure method "pbTo" in {
      using(smallStrings) in {
        data => data.map(_.pbTo[Message[String]])
      }
    }
  }
  
  private val largeMapsBytes = for (count <- counts)
    yield Message((0 until count).map(c => c.toString -> c.toString).toMap).toPB
  
  performance of "a large collection.Map" in {
    measure method "pbTo" in {
      using(largeMapsBytes) in {
        largeMapBytes => largeMapBytes.pbTo[Message[scala.collection.Map[String,String]]]
      }
    }
  }

  performance of "a large Map" in {
    measure method "pbTo" in {
      using(largeMapsBytes) in {
        largeMapBytes => largeMapBytes.pbTo[Message[Map[String,String]]]
      }
    }
  }

  performance of "a large Seq[(String, String)]" in {
    measure method "pbTo" in {
      using(largeMapsBytes) in {
        largeMapBytes => largeMapBytes.pbTo[Message[Seq[(String, String)]]]
      }
    }
  }
}
