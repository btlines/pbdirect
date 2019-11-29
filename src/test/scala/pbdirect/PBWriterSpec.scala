/*
 * Copyright (c) 2019 Beyond the lines
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package pbdirect

import cats.instances.option._
import cats.instances.list._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class PBWriterSpec extends AnyWordSpecLike with Matchers {
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
      message.toPB shouldBe Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116,
        119, 111)
    }
    "write a scala.collection.Map to Protobuf" in {
      case class MapMessage(values: collection.Map[Int, String])
      val message = MapMessage(collection.Map(1 -> "one", 2 -> "two"))
      message.toPB shouldBe Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 7, 8, 2, 18, 3, 116,
        119, 111)
    }
    "write a nested message to Protobuf" in {
      case class InnerMessage(value: Option[Int])
      case class OuterMessage(text: Option[String], inner: Option[InnerMessage])
      val message = OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
      message.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
    }
    "write a sealed trait to Protobuf" in {
      sealed trait Message
      case class IntMessage(value: Option[Int])       extends Message
      case class StringMessage(value: Option[String]) extends Message
      val intMessage: Message    = IntMessage(Some(5))
      val stringMessage: Message = StringMessage(Some("Hello"))
      intMessage.toPB shouldBe Array[Byte](8, 5)
      stringMessage.toPB shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write a message with repeated nested message in Protobuf" in {
      case class Metric(
          metric: String,
          microservice: String,
          node: String,
          value: Float,
          count: Int)
      case class Metrics(metrics: List[Metric])
      val message = Metrics(
        Metric("metric", "microservices", "node", 12F, 12345) :: Nil
      )
      message.toPB shouldBe Array[Byte](10, 37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109,
        105, 99, 114, 111, 115, 101, 114, 118, 105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0,
        0, 64, 65, 40, -71, 96)
    }
    "derive new instance using contramap" in {
      import java.time.Instant
      import cats.syntax.contravariant._
      case class Message(instant: Instant)
      implicit val instantWriter: PBWriter[Instant] = PBWriter[Long].contramap(_.toEpochMilli)
      val instant                                   = Instant.ofEpochMilli(1499411227777L)
      Message(instant).toPB shouldBe Array[Byte](8, -127, -55, -2, -34, -47, 43)
    }
  }
}
