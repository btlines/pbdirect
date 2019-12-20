package pbdirect

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class PBMessageReaderSpec extends AnyWordSpecLike with Matchers {

  "PBMessageReader" should {
    "read an empty message from Protobuf" in {
      case class EmptyMessage()
      val bytes = Array[Byte]()
      bytes.pbTo[EmptyMessage] shouldBe EmptyMessage()
    }
    "read a single-field message from Protobuf" in {
      case class RequiredMessage(@pbIndex(1) value: Int)
      val bytes = Array[Byte](8, 5)
      bytes.pbTo[RequiredMessage] shouldBe RequiredMessage(5)
    }
    "read a multi-field message from Protobuf" in {
      case class MultiMessage(@pbIndex(1) text: Option[String], @pbIndex(2) number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
    "read a multi-field message without pbIndex annotations from Protobuf" in {
      case class MultiMessage(text: Option[String], number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 16, 3)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
    "read a message with missing field from Protobuf" in {
      case class MissingMessage(@pbIndex(1) text: Option[String], @pbIndex(2) number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[MissingMessage] shouldBe MissingMessage(Some("Hello"), None)
    }
    "read a message with repeated field from Protobuf" in {
      case class RepeatedMessage(@pbIndex(1) values: List[Int])
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      bytes.pbTo[RepeatedMessage] shouldBe RepeatedMessage(1 :: 2 :: 3 :: 4 :: Nil)
    }
    "read a Map from Protobuf" in {
      case class MapMessage(@pbIndex(1) values: Map[Int, String])
      val bytes = Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
      bytes.pbTo[MapMessage] shouldBe MapMessage(Map(1 -> "one", 2 -> "two"))
    }
    "read a nested message from Protobuf" in {
      case class InnerMessage(@pbIndex(1) value: Option[Int])
      case class OuterMessage(
          @pbIndex(1) text: Option[String],
          @pbIndex(2) inner: Option[InnerMessage])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
      bytes.pbTo[OuterMessage] shouldBe OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
    }
    "read a sealed trait from Protobuf" in {
      sealed trait Message
      case class IntMessage(@pbIndex(1) value: Option[Int])       extends Message
      case class StringMessage(@pbIndex(1) value: Option[String]) extends Message
      val intBytes    = Array[Byte](8, 5)
      val stringBytes = Array[Byte](10, 5, 72, 101, 108, 108, 111)
      intBytes.pbTo[Message] shouldBe IntMessage(Some(5))
      stringBytes.pbTo[Message] shouldBe StringMessage(Some("Hello"))
    }
    "read a message with repeated nested message from Protobuf" in {
      case class Metric(
          @pbIndex(1) name: String,
          @pbIndex(2) service: String,
          @pbIndex(3) node: String,
          @pbIndex(4) value: Float,
          @pbIndex(5) count: Int)
      case class Metrics(@pbIndex(1) metrics: List[Metric])
      val message = Metrics(
        Metric("metric", "microservices", "node", 12F, 12345) :: Nil
      )
      val bytes = Array[Byte](10, 37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109, 105, 99, 114,
        111, 115, 101, 114, 118, 105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0, 0, 64, 65, 40,
        -71, 96)
      bytes.pbTo[Metrics] shouldBe message
    }
    "read a message with fields out-of-order from Protobuf" in {
      case class MultiMessage(@pbIndex(1) text: Option[String], @pbIndex(2) number: Option[Int])
      val bytes = Array[Byte](16, 3, 10, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
    "read a message with non-sequential field indices from Protobuf" in {
      case class MultiMessage(@pbIndex(1) text: Option[String], @pbIndex(3) number: Option[Int])
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 24, 3)
      bytes.pbTo[MultiMessage] shouldBe MultiMessage(Some("Hello"), Some(3))
    }
  }
}
