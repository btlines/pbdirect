/*
 * Copyright (c) 2017-2020 47 Degrees Open Source <https://www.47deg.com>
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

// Copyright (c) 2017-2020 47 Degrees Open Source <https://www.47deg.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package pbdirect

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import shapeless._
import shapeless.syntax.inject._
import shapeless.tag.@@

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
          @pbIndex(2) inner: Option[InnerMessage]
      )
      val bytes = Array[Byte](10, 5, 72, 101, 108, 108, 111, 18, 2, 8, 11)
      bytes.pbTo[OuterMessage] shouldBe OuterMessage(Some("Hello"), Some(InnerMessage(Some(11))))
    }
    "read a message with repeated nested message from Protobuf" in {
      case class Metric(
          @pbIndex(1) name: String,
          @pbIndex(2) service: String,
          @pbIndex(3) node: String,
          @pbIndex(4) value: Float,
          @pbIndex(5) count: Int
      )
      case class Metrics(@pbIndex(1) metrics: List[Metric])
      val message = Metrics(
        Metric("metric", "microservices", "node", 12f, 12345) :: Nil
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

    type Cop = Int :+: String :+: Boolean :+: CNil
    case class CoproductMessage(
        @pbIndex(1) a: Int,
        @pbIndex(3, 5, 7) b: Option[Cop]
    )
    "read a properly annotated message with a Coproduct field (1st branch)" in {
      val bytes = Array[Byte](8, 5, 24, 9)
      bytes.pbTo[CoproductMessage] shouldBe CoproductMessage(5, Some(9.inject[Cop]))
    }
    "read a properly annotated message with a Coproduct field (2nd branch)" in {
      val bytes = Array[Byte](8, 5, 42, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[CoproductMessage] shouldBe CoproductMessage(5, Some("Hello".inject[Cop]))
    }
    "read a properly annotated message with a Coproduct field (3rd branch)" in {
      val bytes = Array[Byte](8, 5, 56, 1)
      bytes.pbTo[CoproductMessage] shouldBe CoproductMessage(5, Some(true.inject[Cop]))
    }
    "read a properly annotated message with a Coproduct field (field is missing)" in {
      val bytes = Array[Byte](8, 5)
      bytes.pbTo[CoproductMessage] shouldBe CoproductMessage(5, None)
    }
    "read a oneof field with the default value" in {
      val bytes = Array[Byte](8, 5, 42, 0)
      bytes.pbTo[CoproductMessage] shouldBe CoproductMessage(5, Some("".inject[Cop]))
    }
    "read a message with an embedded message containing a oneof field" in {
      case class WrapperMessage(@pbIndex(3) embedded: CoproductMessage)
      val bytes = Array[Byte](26, 4, 8, 5, 42, 0)
      bytes.pbTo[WrapperMessage] shouldBe WrapperMessage(CoproductMessage(5, Some("".inject[Cop])))
    }
    case class EitherMessage(
        @pbIndex(1) a: Int,
        @pbIndex(3, 5) either: Option[Either[String, Int]]
    )
    "read a properly annotated message with an Either field (left)" in {
      val bytes = Array[Byte](8, 5, 26, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[EitherMessage] shouldBe EitherMessage(5, Some(Left("Hello")))
    }
    "write a properly annotated message with an Either field (right)" in {
      val bytes = Array[Byte](8, 5, 40, 8)
      bytes.pbTo[EitherMessage] shouldBe EitherMessage(5, Some(Right(8)))
    }
    "read a message with a signed int field" in {
      case class SignedIntMessage(a: Int @@ Signed, b: String)
      val bytes = Array[Byte](8, 9, 18, 5, 72, 101, 108, 108, 111)
      bytes.pbTo[SignedIntMessage] shouldBe SignedIntMessage(tag[Signed](-5), "Hello")
    }
  }
}
