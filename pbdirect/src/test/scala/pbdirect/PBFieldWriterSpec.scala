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
import java.io.ByteArrayOutputStream
import com.google.protobuf.CodedOutputStream
import pbdirect.PBFieldWriter.Flags
import shapeless.tag
import shapeless.tag._

class PBFieldWriterSpec extends AnyWordSpecLike with Matchers {

  def write[A](value: A, flags: Flags = Flags(skipDefaultValue = true, unpacked = false))(implicit
      writer: PBFieldWriter[A]
  ): Array[Byte] = {
    val buffer = new ByteArrayOutputStream()
    val out    = CodedOutputStream.newInstance(buffer)
    writer.writeTo(1, value, out, flags)
    out.flush()
    buffer.toByteArray()
  }

  "PBFieldWriter" should {
    "write a Boolean to Protobuf" in {
      write(true) shouldBe Array[Byte](8, 1)
    }
    "write a Byte to Protobuf" in {
      write(32: Byte) shouldBe Array[Byte](8, 32)
    }
    "write a Short to Protobuf" in {
      write(8191: Short) shouldBe Array[Byte](8, -1, 63)
    }
    "write an Int to Protobuf" in {
      write(5) shouldBe Array[Byte](8, 5)
    }
    "write an unsigned Int to Protobuf" in {
      write[Int @@ Unsigned](tag.apply(5)) shouldBe Array[Byte](8, 5)
    }
    "write a signed Int to Protobuf" in {
      write[Int @@ Signed](tag.apply(-5)) shouldBe Array[Byte](8, 9)
    }
    "write a fixed-width Int to Protobuf" in {
      write[Int @@ Fixed](tag.apply(5)) shouldBe Array[Byte](13, 5, 0, 0, 0)
    }
    "write a signed fixed-width Int to Protobuf" in {
      // In the Java protobuf implementation, writeSFixed32NoTag(Int)
      // just delegates to writeFixed32NoTag(Int), so the result is
      // the same as the test above.
      write[Int @@ (Signed with Fixed)](tag.apply(5)) shouldBe Array[Byte](13, 5, 0, 0, 0)
    }
    "write a Long to Protobuf" in {
      write(Int.MaxValue.toLong + 1) shouldBe Array[Byte](8, -128, -128, -128, -128, 8)
    }
    "write an unsigned Long to Protobuf" in {
      write[Long @@ Unsigned](tag.apply(Int.MaxValue.toLong + 1)) shouldBe Array[Byte](8, -128,
        -128, -128, -128, 8)
    }
    "write a signed Long to Protobuf" in {
      write[Long @@ Signed](tag.apply(Int.MaxValue.toLong + 1)) shouldBe Array[Byte](8, -128, -128,
        -128, -128, 16)
    }
    "write a fixed-width Long to Protobuf" in {
      write[Long @@ Fixed](tag.apply(Int.MaxValue.toLong + 1)) shouldBe Array[Byte](9, 0, 0, 0,
        -128, 0, 0, 0, 0)
    }
    "write a signed fixed-width Long to Protobuf" in {
      // In the Java protobuf implementation, writeSFixed64NoTag(Long)
      // just delegates to writeFixed64NoTag(Long), so the result is
      // the same as the test above.
      write[Long @@ (Signed with Fixed)](tag.apply(Int.MaxValue.toLong + 1)) shouldBe Array[Byte](9,
        0, 0, 0, -128, 0, 0, 0, 0)
    }
    "write a Float to Protobuf" in {
      write(0.2f) shouldBe Array[Byte](13, -51, -52, 76, 62)
    }
    "write a Double to Protobuf" in {
      write(0.00000000002d) shouldBe Array[Byte](9, -107, 100, 121, -31, 127, -3, -75, 61)
    }
    "write a String to Protobuf" in {
      write("Hello") shouldBe Array[Byte](10, 5, 72, 101, 108, 108, 111)
    }
    "write bytes to Protobuf" in {
      write(Array[Byte](8, 4)) shouldBe Array[Byte](10, 2, 8, 4)
    }
    "write an enumeration to Protobuf" in {
      object Grade extends Enumeration {
        val GradeA, GradeB = Value
      }
      write(Grade.GradeA) shouldBe Array[Byte]() // skip field because default value
      write(Grade.GradeB) shouldBe Array[Byte](8, 1)
    }
    "write an enum to Protobuf" in {
      sealed trait Grade extends Pos
      case object GradeA extends Grade with Pos._0
      case object GradeB extends Grade with Pos._1
      write(GradeA: Grade) shouldBe Array[Byte]() // skip field because default value
      write(GradeB: Grade) shouldBe Array[Byte](8, 1)
    }
    "write an enumeratum IntEnumEntry to Protobuf" in {
      import enumeratum.values._
      sealed abstract class Quality(val value: Int) extends IntEnumEntry
      object Quality extends IntEnum[Quality] {
        case object Good extends Quality(0)
        case object OK   extends Quality(3)
        case object Bad  extends Quality(5)

        val values = findValues
      }
      write(Quality.Good: Quality) shouldBe Array[Byte]() // skip field because default value
      write(Quality.OK: Quality) shouldBe Array[Byte](8, 3)
    }
    "write an Option[Int] to Protobuf" in {
      write(Option(5)) shouldBe Array[Byte](8, 5)
    }
    "write an empty Option[Int] to Protobuf" in {
      write(None: Option[Int]) shouldBe Array[Byte]()
    }
    "write a List[Int] to Protobuf as a packed repeated field" in {
      write(1 :: 2 :: 3 :: 4 :: Nil) shouldBe Array[Byte](10, 4, 1, 2, 3, 4)
    }
    "write an empty List[Int] to Protobuf as a packed repeated field" in {
      write(Nil: List[Int]) shouldBe Array[Byte]()
    }
    "write a List of signed ints to Protobuf as a packed repeated field" in {
      val list: List[Int @@ Signed] = List(1, 2, 3, 4).map(tag[Signed](_))
      write(list) shouldBe Array[Byte](10, 4, 2, 4, 6, 8)
    }
    "write a List[Int] to Protobuf as an unpacked repeated field" in {
      val flags = Flags(skipDefaultValue = true, unpacked = true)
      write(1 :: 2 :: 3 :: 4 :: Nil, flags) shouldBe Array[Byte](8, 1, 8, 2, 8, 3, 8, 4)
    }
    "write an empty List[Int] to Protobuf as an unpacked repeated field" in {
      val flags = Flags(skipDefaultValue = true, unpacked = true)
      write(Nil: List[Int], flags) shouldBe Array[Byte]()
    }
    "write a Seq to Protobuf" in {
      write(Seq(1, 2, 3, 4)) shouldBe Array[Byte](10, 4, 1, 2, 3, 4)
    }
    "write a Map to Protobuf" in {
      write(Map(1 -> "one", 2 -> "two")) shouldBe Array[Byte](10, 7, 8, 1, 18, 3, 111, 110, 101, 10,
        7, 8, 2, 18, 3, 116, 119, 111)
    }
    "write a Map with signed keys to Protobuf" in {
      val map: Map[Int @@ Signed, String] = Map(
        tag[Signed](1) -> "one",
        tag[Signed](2) -> "two"
      )
      write(map) shouldBe Array[Byte](10, 7, 8, 2, 18, 3, 111, 110, 101, 10, 7, 8, 4, 18, 3, 116,
        119, 111)
    }
    "write a scala.collection.Map to Protobuf" in {
      write(collection.Map(1 -> "one", 2 -> "two")) shouldBe Array[Byte](10, 7, 8, 1, 18, 3, 111,
        110, 101, 10, 7, 8, 2, 18, 3, 116, 119, 111)
    }
    "write an embedded message to Protobuf" in {
      case class EmbeddedMessage(@pbIndex(1) value: Int)
      write(EmbeddedMessage(11)) shouldBe Array[Byte](10, 2, 8, 11)
    }
    "write an embedded message with all fields missing to Protobuf" in {
      case class EmbeddedMessage(@pbIndex(1) value: Option[Int])
      write(EmbeddedMessage(None)) shouldBe Array[Byte](10, 0)
    }
    "write a repeated embedded message in Protobuf" in {
      case class Metric(
          @pbIndex(1) metric: String,
          @pbIndex(2) microservice: String,
          @pbIndex(3) node: String,
          @pbIndex(4) value: Float,
          @pbIndex(5) count: Int
      )
      write(Metric("metric", "microservices", "node", 12f, 12345) :: Nil) shouldBe Array[Byte](10,
        37, 10, 6, 109, 101, 116, 114, 105, 99, 18, 13, 109, 105, 99, 114, 111, 115, 101, 114, 118,
        105, 99, 101, 115, 26, 4, 110, 111, 100, 101, 37, 0, 0, 64, 65, 40, -71, 96)
    }
    "skip a Boolean field with the default value" in {
      write(false) shouldBe Array[Byte]()
    }
    "skip an Int field with the default value" in {
      write(0) shouldBe Array[Byte]()
    }
    "skip a String field with the default value" in {
      write("") shouldBe Array[Byte]()
    }
    "not skip default values in a repeated field" in {
      write(List[Int](1, 0, 2)) shouldBe Array[Byte](10, 3, 1, 0, 2)
    }
    "not skip default keys or values in a map field" in {
      write(Map[Int, String](0 -> "zero", 1 -> "one", 2 -> "")) shouldBe
        Array(10, 8, 8, 0, 18, 4, 122, 101, 114, 111, 10, 7, 8, 1, 18, 3, 111, 110, 101, 10, 4, 8,
          2, 18, 0)
    }
  }
}
