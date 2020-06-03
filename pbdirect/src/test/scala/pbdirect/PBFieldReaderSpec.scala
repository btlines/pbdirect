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
import enumeratum.values._
import shapeless.tag.@@

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
    "read an unsigned Int from Protobuf" in {
      val bytes = Array[Byte](8, 5)
      PBFieldReader[Int @@ Unsigned].read(1, bytes) shouldBe 5
    }
    "read a signed Int from Protobuf" in {
      val bytes = Array[Byte](8, 9)
      PBFieldReader[Int @@ Signed].read(1, bytes) shouldBe -5
    }
    "read a fixed-width Int from Protobuf" in {
      val bytes = Array[Byte](13, 5, 0, 0, 0)
      PBFieldReader[Int @@ Fixed].read(1, bytes) shouldBe 5
    }
    "read a signed fixed-width Int from Protobuf" in {
      // sfixed32 is encoded exactly the same way as fixed32,
      // so the bytes are the same as in the test above
      val bytes = Array[Byte](13, 5, 0, 0, 0)
      PBFieldReader[Int @@ (Signed with Fixed)].read(1, bytes) shouldBe 5
    }
    "read a Long from Protobuf" in {
      val bytes = Array[Byte](8, -128, -128, -128, -128, 8)
      PBFieldReader[Long].read(1, bytes) shouldBe Int.MaxValue.toLong + 1
    }
    "read an unsigned Long from Protobuf" in {
      val bytes = Array[Byte](8, -128, -128, -128, -128, 8)
      PBFieldReader[Long @@ Unsigned].read(1, bytes) shouldBe Int.MaxValue.toLong + 1
    }
    "read a signed Long from Protobuf" in {
      val bytes = Array[Byte](8, -128, -128, -128, -128, 16)
      PBFieldReader[Long @@ Signed].read(1, bytes) shouldBe Int.MaxValue.toLong + 1
    }
    "read a fixed-width Long from Protobuf" in {
      val bytes = Array[Byte](9, 0, 0, 0, -128, 0, 0, 0, 0)
      PBFieldReader[Long @@ Fixed].read(1, bytes) shouldBe Int.MaxValue.toLong + 1
    }
    "read a signed fixed-width Long from Protobuf" in {
      // sfixed64 is encoded exactly the same way as fixed64,
      // so the bytes are the same as in the test above
      val bytes = Array[Byte](9, 0, 0, 0, -128, 0, 0, 0, 0)
      PBFieldReader[Long @@ (Signed with Fixed)].read(1, bytes) shouldBe Int.MaxValue.toLong + 1
    }
    "read a Float from Protobuf" in {
      val bytes = Array[Byte](13, -51, -52, 76, 62)
      PBFieldReader[Float].read(1, bytes) shouldBe 0.2f
    }
    "read a Double from Protobuf" in {
      val bytes = Array[Byte](9, -107, 100, 121, -31, 127, -3, -75, 61)
      PBFieldReader[Double].read(1, bytes) shouldBe 0.00000000002d
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
    "read an unpacked repeated field from Protobuf" in {
      val bytes = Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
      PBFieldReader[List[Int]].read(1, bytes) shouldBe 1 :: 2 :: 3 :: 4 :: Nil
    }
    "read a packed repeated field from Protobuf" in {
      val bytes = Array[Byte](10, 4, 1, 2, 3, 4)
      PBFieldReader[List[Int]].read(1, bytes) shouldBe 1 :: 2 :: 3 :: 4 :: Nil
    }
    "read a packed repeated field split into multiple key-value pairs" in {
      val bytes = Array[Byte](10, 2, 1, 2, 10, 2, 3, 4)
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
          @pbIndex(5) count: Int
      )
      val bytes = Array[Byte](10, 37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109, 105, 99, 114,
        111, 115, 101, 114, 118, 105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0, 0, 64, 65, 40,
        -71, 96)
      PBFieldReader[List[Metric]]
        .read(1, bytes) shouldBe Metric("metric", "microservices", "node", 12f, 12345) :: Nil
    }
    "use the default value when reading a missing Boolean from Protobuf" in {
      PBFieldReader[Boolean].read(1, Array.empty[Byte]) shouldBe false
    }
    "use the default value when reading a missing Byte from Protobuf" in {
      PBFieldReader[Byte].read(1, Array.empty[Byte]) shouldBe 0.toByte
    }
    "use the default value when reading a missing Short from Protobuf" in {
      PBFieldReader[Short].read(1, Array.empty[Byte]) shouldBe 0.toShort
    }
    "use the default value when reading a missing Int from Protobuf" in {
      PBFieldReader[Int].read(1, Array.empty[Byte]) shouldBe 0
    }
    "use the default value when reading a missing Long from Protobuf" in {
      PBFieldReader[Long].read(1, Array.empty[Byte]) shouldBe 0L
    }
    "use the default value when reading a missing Float from Protobuf" in {
      PBFieldReader[Float].read(1, Array.empty[Byte]) shouldBe 0.0f
    }
    "use the default value when reading a missing Double from Protobuf" in {
      PBFieldReader[Double].read(1, Array.empty[Byte]) shouldBe 0.0
    }
    "use the default value when reading a missing String from Protobuf" in {
      PBFieldReader[String].read(1, Array.empty[Byte]) shouldBe ""
    }
    "use the default value when reading a missing byte array from Protobuf" in {
      PBFieldReader[Array[Byte]].read(1, Array.empty[Byte]) shouldBe Array.empty[Byte]
    }
    "use the default value when reading a missing enumeration from Protobuf" in {
      case object Grade extends Enumeration {
        val GradeA, GradeB = Value
      }
      PBFieldReader[Grade.Value].read(1, Array.empty[Byte]) shouldBe Grade.GradeA
    }
    "use the default value when reading a missing enum from Protobuf" in {
      sealed trait Grade extends Pos
      case object GradeA extends Grade with Pos._0
      case object GradeB extends Grade with Pos._1
      PBFieldReader[Grade].read(1, Array.empty[Byte]) shouldBe GradeA
    }
    "use the default value when reading a missing enumeratum IntEnumEntry from Protobuf" in {
      PBFieldReader[Quality].read(1, Array.empty[Byte]) shouldBe Quality.Good
    }
    "use the default value when reading a missing embedded message from Protobuf" in {
      case class FurtherEmbeddedMessage(value: Int)
      case class EmbeddedMessage(a: Int, b: String, c: FurtherEmbeddedMessage)
      PBFieldReader[EmbeddedMessage]
        .read(1, Array.empty[Byte]) shouldBe EmbeddedMessage(0, "", FurtherEmbeddedMessage(0))
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
