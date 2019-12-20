package pbdirect

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import enumeratum.values._

class PBFieldReaderSpec extends AnyWordSpecLike with Matchers {

  "PBFieldReader" should {
    "read a Boolean from Protobuf" in {
      val bytes = Array[Byte](8, 1)
      PBFieldReader[Boolean].read(1, bytes) shouldBe true
    }
    "read a Byte from Protobuf" in {
      val bytes = Array[Byte](8, 32)
      PBFieldReader[Byte].read(1, bytes) shouldBe 32.toByte
    }
    "read a Short from Protobuf" in {
      val bytes = Array[Byte](8, -1, 63)
      PBFieldReader[Short].read(1, bytes) shouldBe 8191.toShort
    }
    "read an Int from Protobuf" in {
      val bytes = Array[Byte](8, 5)
      PBFieldReader[Int].read(1, bytes) shouldBe 5
    }
    "read a Long from Protobuf" in {
      val bytes = Array[Byte](8, -128, -128, -128, -128, 8)
      PBFieldReader[Long].read(1, bytes) shouldBe Int.MaxValue.toLong + 1
    }
    "read a Float from Protobuf" in {
      val bytes = Array[Byte](13, -51, -52, 76, 62)
      PBFieldReader[Float].read(1, bytes) shouldBe 0.2F
    }
    "read a Double from Protobuf" in {
      val bytes = Array[Byte](9, -107, 100, 121, -31, 127, -3, -75, 61)
      PBFieldReader[Double].read(1, bytes) shouldBe 0.00000000002D
    }
    "read a String from Protobuf" in {
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      PBFieldReader[String].read(1, bytes) shouldBe "Hello"
    }
    "read bytes from Protobuf" in {
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      PBFieldReader[Array[Byte]].read(1, bytes) shouldBe Array[Byte](72, 101, 108, 108, 111)
    }
    "read an enumeration from Protobuf" in {
      case object Grade extends Enumeration {
        val GradeA, GradeB = Value
      }
      val bytesA = Array[Byte](8, 0)
      val bytesB = Array[Byte](8, 1)
      PBFieldReader[Grade.Value].read(1, bytesA) shouldBe Grade.GradeA
      PBFieldReader[Grade.Value].read(1, bytesB) shouldBe Grade.GradeB
    }
    "read an enum from Protobuf" in {
      sealed trait Grade extends Pos
      case object GradeA extends Grade with Pos._0
      case object GradeB extends Grade with Pos._1
      val bytesA = Array[Byte](8, 0)
      val bytesB = Array[Byte](8, 1)
      PBFieldReader[Grade].read(1, bytesA) shouldBe GradeA
      PBFieldReader[Grade].read(1, bytesB) shouldBe GradeB
    }
    "read an enumeratum IntEnumEntry from Protobuf" in {
      val bytes = Array[Byte](8, 3)
      PBFieldReader[Quality].read(1, bytes) shouldBe Quality.OK
    }
    "read an optional field (missing) from Protobuf" in {
      val bytes = Array[Byte]()
      PBFieldReader[Option[Int]].read(1, bytes) shouldBe None
    }
    "read an optional field (populated) from Protobuf" in {
      val bytes = Array[Byte](8, 11)
      PBFieldReader[Option[Int]].read(1, bytes) shouldBe Some(11)
    }
    "read a repeated field from Protobuf" in {
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      PBFieldReader[List[Int]].read(1, bytes) shouldBe 1 :: 2 :: 3 :: 4 :: Nil
    }
    "read a Seq from Protobuf" in {
      case class RepeatedMessage(@pbIndex(1) values: Seq[Int])
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      PBFieldReader[Seq[Int]].read(1, bytes) shouldBe Seq(1, 2, 3, 4)
    }
    "read a Map from Protobuf" in {
      val bytes = Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
      PBFieldReader[Map[Int, String]].read(1, bytes) shouldBe Map(1 -> "one", 2 -> "two")
    }
    "read a scala.collection.Map from Protobuf" in {
      val bytes = Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
      PBFieldReader[collection.Map[Int, String]]
        .read(1, bytes) shouldBe collection.Map(1 -> "one", 2 -> "two")
    }
    "read an embedded message from Protobuf" in {
      case class EmbeddedMessage(@pbIndex(1) value: Option[Int])
      val bytes = Array[Byte](10, 2, 8, 11)
      PBFieldReader[EmbeddedMessage].read(1, bytes) shouldBe EmbeddedMessage(Some(11))
    }
    "read a repeated embedded message from Protobuf" in {
      case class Metric(
          @pbIndex(1) name: String,
          @pbIndex(2) service: String,
          @pbIndex(3) node: String,
          @pbIndex(4) value: Float,
          @pbIndex(5) count: Int)
      val bytes = Array[Byte](10, 37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109, 105, 99, 114,
        111, 115, 101, 114, 118, 105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0, 0, 64, 65, 40,
        -71, 96)
      PBFieldReader[List[Metric]]
        .read(1, bytes) shouldBe Metric("metric", "microservices", "node", 12F, 12345) :: Nil
    }
  }
}

// For some reason it fails to resolve the implicit PBReader if the enum is defined inside the test class
sealed abstract class Quality(val value: Int) extends IntEnumEntry

object Quality extends IntEnum[Quality] {
  case object Good extends Quality(0)
  case object OK   extends Quality(3)
  case object Bad  extends Quality(5)

  val values = findValues
}
