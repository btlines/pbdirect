package pbdirect

import org.scalameter.Gen
import org.scalameter.api.Bench

object PBReaderBenchmark extends Bench.LocalTime {
  
  /////////////////////////////////////////////////////////////////////////////
  // The following microbenchmarking tests a very specific scenario: Reading
  // a case class with many fields.
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
}
