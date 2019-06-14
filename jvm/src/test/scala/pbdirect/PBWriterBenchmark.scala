package pbdirect

import org.scalameter.{Bench, Gen}

object PBWriterBenchmark extends Bench.LocalTime {

  /////////////////////////////////////////////////////////////////////////////
  // The following microbenchmarking tests a very specific scenario: A deeply-
  // nested case class or tuple contains a payload of bytes at the end.
  // Here we are specifically interested in how serialization performance
  // scales with the depth of the payload.  (Ideally it wouldn't matter much
  // how deep it is, but see https://github.com/btlines/pbdirect/issues/39.)
  /////////////////////////////////////////////////////////////////////////////
  
  private def make5DeepTuple[V](rootValue: V): Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[V]]]]] = {
    Tuple1(Tuple1(Tuple1(Tuple1(Tuple1(rootValue)))))
  }
  private def make10DeepTuple[V](rootValue: V): Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[V]]]]]]]]]] = {
    make5DeepTuple(make5DeepTuple(rootValue))
  }
  private def make20DeepTuple[V](rootValue: V): Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[Tuple1[V]]]]]]]]]]]]]]]]]]]] = {
    make10DeepTuple(make10DeepTuple(rootValue))
  }

  private val sizes = Gen.range("size")(100000, 1000000, 100000)
  private def makePayload(size: Int) = Array.fill(size)(1.toByte)
  
  private val fiveDeep = for (size <- sizes) yield make5DeepTuple(makePayload(size))
  private val tenDeep = for (size <- sizes) yield make10DeepTuple(makePayload(size))
  private val twentyDeep = for (size <- sizes) yield make20DeepTuple(makePayload(size))
  
  performance of "5-Deep Tuple" in {
    measure method "toPB" in {
      using(fiveDeep) in {
        d => d.toPB
      }
    }
  }

  performance of "10-Deep Tuple" in {
    measure method "toPB" in {
      using(tenDeep) in {
        d => d.toPB
      }
    }
  }

  performance of "20-Deep Tuple" in {
    measure method "toPB" in {
      using(twentyDeep) in {
        d => d.toPB
      }
    }
  }
}
