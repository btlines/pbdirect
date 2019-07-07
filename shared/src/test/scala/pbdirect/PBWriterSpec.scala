package pbdirect

import cats.instances.option._
import cats.instances.list._
import org.scalatest.{Matchers, WordSpecLike}

class PBWriterSpec extends WordSpecLike with Matchers {
  "PBWriter" should {
    "write a Boolean to Protobuf" in {
      case class BooleanMessage(value: Option[Boolean])
      val message = BooleanMessage(Some(true))
      message.toPB shouldBe Array[Byte](8, 1)
    }
    "write a Byte to Protobuf" in {
      case class ByteMessage(value: Option[Byte])
      val message = ByteMessage(Some(32))
      message.toPB shouldBe Array[Byte](8, 32)
    }
    "write a Short to Protobuf" in {
      case class ShortMessage(value: Option[Short])
      val message = ShortMessage(Some(8191))
      message.toPB shouldBe Array[Byte](8, -1, 63)
    }
    "write an Int to Protobuf" in {
      case class IntMessage(value: Option[Int])
      val message = IntMessage(Some(5))
      message.toPB shouldBe Array[Byte](8, 5)
    }
    "write a multi-byte Int to Protobuf" in {
      case class IntMessage(value: Option[Int])
      val message = IntMessage(Some(150))
      message.toPB shouldBe Array[Byte](8, -106, 1)
    }
    "write a Long to Protobuf" in {
      case class LongMessage(value: Option[Long])
      val message = LongMessage(Some(Int.MaxValue.toLong + 1))
      message.toPB shouldBe Array[Byte](8, -128, -128, -128, -128, 8)
    }
    "write a Float to Protobuf" in {
      case class FloatMessage(value: Option[Float])
      val message = FloatMessage(Some(0.2F))
      message.toPB shouldBe Array[Byte](13, -51, -52, 76, 62)
    }
    "write a Double to Protobuf" in {
      case class DoubleMessage(value: Option[Double])
      val message = DoubleMessage(Some(0.00000000002D))
      message.toPB shouldBe Array[Byte](9, -107, 100, 121, -31, 127, -3, -75, 61)
    }
    "write a String to Protobuf" in {
      case class StringMessage(value: Option[String])
      val message = StringMessage(Some("Hello"))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write bytes to Protobuf" in {
      case class BytesMessage(value: Array[Byte])
      val message = BytesMessage(Array[Byte](8, 4))
      message.toPB shouldBe Array[Byte](10, 2, 8, 4)
    }
    "write an enumeration to Protobuf" in {
      object Grade extends Enumeration {
        val GradeA, GradeB = Value
      }
      case class EnumMessage(value: Grade.Value)
      val message = EnumMessage(Grade.GradeB)
      message.toPB shouldBe Array[Byte](8, 1)
    }
    "write an enum to Protobuf" in {
      sealed trait Grade extends Pos
      case object GradeA extends Grade with Pos._0
      case object GradeB extends Grade with Pos._1
      case class GradeMessage(value: Option[Grade])
      val messageA = GradeMessage(Some(GradeA))
      val messageB = GradeMessage(Some(GradeB))
      messageA.toPB shouldBe Array[Byte](8, 0)
      messageB.toPB shouldBe Array[Byte](8, 1)
    }
    "write a required field to Protobuf" in {
      case class RequiredMessage(value: Int)
      val message = RequiredMessage(5)
      message.toPB shouldBe Array[Byte](8, 5)
    }
    "write an empty message to Protobuf" in {
      case class EmptyMessage()
      val message = EmptyMessage()
      message.toPB shouldBe Array[Byte]()
    }
    "write a message with a single missing field to Protobuf" in {
      case class MissingMessage(text: Option[String])
      val message = MissingMessage(None)
      message.toPB shouldBe Array[Byte]()
    }
    "write a multi-field message to Protobuf" in {
      case class MultiMessage(text: Option[String], number: Option[Int])
      val message = MultiMessage(Some("Hello"), Some(3))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
    }
    "write a message with missing field to Protobuf" in {
      case class MissingMessage(text: Option[String], number: Option[Int])
      val message = MissingMessage(Some("Hello"), None)
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write a message with repeated field to Protobuf" in {
      case class RepeatedMessage(values: List[Int])
      val message = RepeatedMessage(1 :: 2 :: 3 :: 4 :: Nil)
      message.toPB shouldBe Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
    }
    "write a message with Seq to Protobuf" in {
      case class RepeatedMessage(values: Seq[Int])
      val message = RepeatedMessage(Seq(1, 2, 3, 4))
      message.toPB shouldBe Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
    }
    "write a Map to Protobuf" in {
      case class MapMessage(values: Map[Int, String])
      val message = MapMessage(Map(1 -> "one", 2 -> "two"))
      message.toPB shouldBe Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
    }
    "write a scala.collection.Map to Protobuf" in {
      case class MapMessage(values: collection.Map[Int, String])
      val message = MapMessage(collection.Map(1 -> "one", 2 -> "two"))
      message.toPB shouldBe Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
    }
    "write a nested message to Protobuf" in {
      case class InnerMessage(value: Option[Int])
      case class OuterMessage(text: Option[String], inner: Option[InnerMessage])
      val message = OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
    }
    "write a sealed trait to Protobuf" in {
      sealed trait Message
      case class IntMessage(value: Option[Int]) extends Message
      case class StringMessage(value: Option[String]) extends Message
      val intMessage: Message = IntMessage(Some(5))
      val stringMessage: Message = StringMessage(Some("Hello"))
      intMessage.toPB shouldBe Array[Byte](8, 5)
      stringMessage.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write a nested sealed trait to Protobuf" in {
      sealed trait Message
      case class IntMessage(value: Option[Int]) extends Message
      case class StringMessage(value: Option[String]) extends Message 
      case class NestedMessage(value: Message)
      val intMessage = NestedMessage(IntMessage(Some(5)))
      intMessage.toPB shouldBe Array[Byte](10, 2, 8, 5)
      val stringMessage = NestedMessage(StringMessage(Some("Hello")))
      stringMessage.toPB shouldBe Array[Byte](10, 7, 10, 5, 72, 101, 108, 108, 111)
    }
    "write a sealed trait with same repr to Protobuf" in {
      sealed trait Message
      case class M1(@Index(1) value: Int) extends Message
      case class M2(@Index(2) value: Int) extends Message
      val m1 = M1(1)
      val m2 = M2(2)
      m1.toPB shouldBe Array[Byte](8, 1)
      m2.toPB shouldBe Array[Byte](16, 2)
    }
    "write a message with repeated nested message in Protobuf" in {
      case class Metric(metric: String, microservice: String, node: String, value: Float, count: Int)
      case class Metrics(metrics: List[Metric])
      val message = Metrics(
        Metric("metric", "microservices", "node", 12F, 12345) :: Nil
      )
      message.toPB shouldBe Array[Byte](10, 37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109, 105, 99, 114, 111, 115,
        101, 114, 118, 105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0, 0, 64, 65, 40, -71, 96)
    }
    "write a message with nested repeated field in Protobuf" in {
      case class ListOfLists(v: List[ListOfInt])
      case class ListOfInt(v: List[Int])
      val message = ListOfLists(List(ListOfInt(List(1, 2)), ListOfInt(List(3, 4))))
      val bytes = Array[Byte](10, 4, 8, 1, 8, 2, 10, 4, 8, 3, 8, 4)
      message.toPB shouldBe bytes
    }
    "write to Protobuf a message with many references to the same object at different indices" in {
      case class Message(value: Option[Int])
      case class LargeMessage(
        value0:  Message, value1:  Message, value2:  Message, value3:  Message,
        value4:  Message, value5:  Message, value6:  Message, value7:  Message,
        value8:  Message, value9:  Message, value10: Message, value11: Message,
        value12: Message, value13: Message, value14: Message, value15: Message)
      case class NestedMessage(value: LargeMessage)
      val message = Message(Some(1))
      val nestedMessage = NestedMessage(LargeMessage(
        message, message, message, message, message, message, message, message,
        message, message, message, message, message, message, message, message))
      nestedMessage.toPB shouldBe Array[Byte](
        10, 65, 10, 2, 8, 1, 18, 2, 8, 1, 26, 2, 8, 1, 34, 2, 8, 1, 42, 2, 8,
        1, 50, 2, 8, 1, 58, 2, 8, 1, 66, 2, 8, 1, 74, 2, 8, 1, 82, 2, 8, 1, 90,
        2, 8, 1, 98, 2, 8, 1, 106, 2, 8, 1, 114, 2, 8, 1, 122, 2, 8, 1, -126,
        1, 2, 8, 1
      )
    }
    "derive new instance using contramap" in {
      import java.time.Instant
      import cats.syntax.contravariant._
      case class Message(instant: Instant)
      implicit val instantWriter: PBWriter[Instant] = PBWriter[Long].contramap(_.toEpochMilli)
      val instant = Instant.ofEpochMilli(1499411227777L)
      Message(instant).toPB shouldBe Array[Byte](8, -127, -55, -2, -34, -47, 43)
    }
  }
}
