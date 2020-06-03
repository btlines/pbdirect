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

import java.io.ByteArrayOutputStream

import cats.Contravariant
import cats.syntax.contravariant._
import com.google.protobuf.CodedOutputStream
import com.google.protobuf.WireFormat._
import enumeratum.values.IntEnumEntry
import shapeless.tag.@@

trait PBScalarValueWriter[A] {

  /**
   * The wire type that this writer uses.
   *
   * 0 = varint
   * 1 = fixed64
   * 2 = length delimited
   * 5 = fixed32
   * (3 and 4 are deprecated and should not be used)
   *
   * @return one of the `WIRETYPE` constants listed in [[com.google.protobuf.WireFormat]]
   */
  def wireType: Int

  /**
   * Whether repeated fields of this type can be packed.
   * Primitive repeated fields (ints, floats, bools and enums)
   * can be packed, and should be, unless overriden using a @pbUnpacked annotation.
   */
  def canBePacked: Boolean =
    Set(WIRETYPE_VARINT, WIRETYPE_FIXED32, WIRETYPE_FIXED64) contains wireType

  /**
   * Whether the given value would be encoded as the default value
   * for its corresponding protobuf scalar type.
   *
   * If the field has the default value, we need to skip it when
   * writing to protobuf.
   *
   * Default values for the protobuf scalar types are as follows:
   *
   * string -> ""
   * bytes -> empty bytes
   * bool -> false
   * all numeric types -> zero
   * enums -> the first defined enum value, which must be 0
   *
   * For embedded message fields, there isn't a sensible default value
   * so this method will always return false.
   */
  def isDefault(value: A): Boolean

  /**
   * Write the value to the output stream *without* prefixing a tag.
   */
  def writeWithoutTag(value: A, out: CodedOutputStream): Unit

}

trait LowPriorityPBScalarValueWriterImplicits {

  implicit def embeddedMessageWriter[A](implicit
      messageWriter: PBMessageWriter[A]
  ): PBScalarValueWriter[A] =
    new PBScalarValueWriter[A] {
      override def wireType: Int                  = WIRETYPE_LENGTH_DELIMITED
      override def isDefault(message: A): Boolean = false
      override def writeWithoutTag(message: A, out: CodedOutputStream): Unit = {
        val buffer    = new ByteArrayOutputStream()
        val bufferOut = CodedOutputStream.newInstance(buffer)
        messageWriter.writeTo(message, bufferOut)
        bufferOut.flush()
        out.writeByteArrayNoTag(buffer.toByteArray)
      }
    }

}

trait PBScalarValueWriterImplicits extends LowPriorityPBScalarValueWriterImplicits {

  implicit object ContravariantWriter extends Contravariant[PBScalarValueWriter] {
    override def contramap[A, B](writer: PBScalarValueWriter[A])(f: B => A) =
      new PBScalarValueWriter[B] {
        override def wireType: Int            = writer.wireType
        override def isDefault(b: B): Boolean = writer.isDefault(f(b))
        override def writeWithoutTag(b: B, out: CodedOutputStream): Unit =
          writer.writeWithoutTag(f(b), out)
      }
  }

  implicit val booleanWriter: PBScalarValueWriter[Boolean] =
    new PBScalarValueWriter[Boolean] {
      override def wireType: Int                      = WIRETYPE_VARINT
      override def isDefault(value: Boolean): Boolean = !value
      override def writeWithoutTag(value: Boolean, out: CodedOutputStream): Unit =
        out.writeBoolNoTag(value)
    }

  implicit val untaggedIntWriter: PBScalarValueWriter[Int] =
    new PBScalarValueWriter[Int] {
      override def wireType: Int                  = WIRETYPE_VARINT
      override def isDefault(value: Int): Boolean = value == 0
      override def writeWithoutTag(value: Int, out: CodedOutputStream): Unit =
        out.writeInt32NoTag(value)
    }

  implicit val unsignedIntWriter: PBScalarValueWriter[Int @@ Unsigned] =
    new PBScalarValueWriter[Int @@ Unsigned] {
      override def wireType: Int                              = WIRETYPE_VARINT
      override def isDefault(value: Int @@ Unsigned): Boolean = value == 0
      override def writeWithoutTag(value: Int @@ Unsigned, out: CodedOutputStream): Unit =
        out.writeUInt32NoTag(value)
    }

  implicit val signedIntWriter: PBScalarValueWriter[Int @@ Signed] =
    new PBScalarValueWriter[Int @@ Signed] {
      override def wireType: Int                            = WIRETYPE_VARINT
      override def isDefault(value: Int @@ Signed): Boolean = value == 0
      override def writeWithoutTag(value: Int @@ Signed, out: CodedOutputStream): Unit =
        out.writeSInt32NoTag(value)
    }

  implicit val fixedIntWriter: PBScalarValueWriter[Int @@ Fixed] =
    new PBScalarValueWriter[Int @@ Fixed] {
      override def wireType: Int                           = WIRETYPE_FIXED32
      override def isDefault(value: Int @@ Fixed): Boolean = value == 0
      override def writeWithoutTag(value: Int @@ Fixed, out: CodedOutputStream): Unit =
        out.writeFixed32NoTag(value)
    }

  implicit val fixedSignedIntWriter: PBScalarValueWriter[Int @@ (Signed with Fixed)] =
    new PBScalarValueWriter[Int @@ (Signed with Fixed)] {
      override def wireType: Int                                         = WIRETYPE_FIXED32
      override def isDefault(value: Int @@ (Signed with Fixed)): Boolean = value == 0
      override def writeWithoutTag(
          value: Int @@ (Signed with Fixed),
          out: CodedOutputStream
      ): Unit =
        out.writeSFixed32NoTag(value)
    }

  implicit val byteWriter: PBScalarValueWriter[Byte] =
    untaggedIntWriter.contramap[Byte](_.toInt)

  implicit val shortWriter: PBScalarValueWriter[Short] =
    untaggedIntWriter.contramap[Short](_.toInt)

  implicit val untaggedLongWriter: PBScalarValueWriter[Long] =
    new PBScalarValueWriter[Long] {
      override def wireType: Int                   = WIRETYPE_VARINT
      override def isDefault(value: Long): Boolean = value == 0L
      override def writeWithoutTag(value: Long, out: CodedOutputStream): Unit =
        out.writeInt64NoTag(value)
    }

  implicit val unsignedLongWriter: PBScalarValueWriter[Long @@ Unsigned] =
    new PBScalarValueWriter[Long @@ Unsigned] {
      override def wireType: Int                               = WIRETYPE_VARINT
      override def isDefault(value: Long @@ Unsigned): Boolean = value == 0L
      override def writeWithoutTag(value: Long @@ Unsigned, out: CodedOutputStream): Unit =
        out.writeUInt64NoTag(value)
    }

  implicit val signedLongWriter: PBScalarValueWriter[Long @@ Signed] =
    new PBScalarValueWriter[Long @@ Signed] {
      override def wireType: Int                             = WIRETYPE_VARINT
      override def isDefault(value: Long @@ Signed): Boolean = value == 0L
      override def writeWithoutTag(value: Long @@ Signed, out: CodedOutputStream): Unit =
        out.writeSInt64NoTag(value)
    }

  implicit val fixedLongWriter: PBScalarValueWriter[Long @@ Fixed] =
    new PBScalarValueWriter[Long @@ Fixed] {
      override def wireType: Int                            = WIRETYPE_FIXED64
      override def isDefault(value: Long @@ Fixed): Boolean = value == 0L
      override def writeWithoutTag(value: Long @@ Fixed, out: CodedOutputStream): Unit =
        out.writeFixed64NoTag(value)
    }

  implicit val fixedSignedLongWriter: PBScalarValueWriter[Long @@ (Signed with Fixed)] =
    new PBScalarValueWriter[Long @@ (Signed with Fixed)] {
      override def wireType: Int                                          = WIRETYPE_FIXED64
      override def isDefault(value: Long @@ (Signed with Fixed)): Boolean = value == 0L
      override def writeWithoutTag(
          value: Long @@ (Signed with Fixed),
          out: CodedOutputStream
      ): Unit =
        out.writeSFixed64NoTag(value)
    }

  implicit val floatWriter: PBScalarValueWriter[Float] =
    new PBScalarValueWriter[Float] {
      override def wireType: Int                    = WIRETYPE_FIXED32
      override def isDefault(value: Float): Boolean = value == 0.0f
      override def writeWithoutTag(value: Float, out: CodedOutputStream): Unit =
        out.writeFloatNoTag(value)
    }

  implicit val doubleWriter: PBScalarValueWriter[Double] =
    new PBScalarValueWriter[Double] {
      override def wireType: Int                     = WIRETYPE_FIXED64
      override def isDefault(value: Double): Boolean = value == 0.0
      override def writeWithoutTag(value: Double, out: CodedOutputStream): Unit =
        out.writeDoubleNoTag(value)
    }

  implicit val stringWriter: PBScalarValueWriter[String] =
    new PBScalarValueWriter[String] {
      override def wireType: Int                     = WIRETYPE_LENGTH_DELIMITED
      override def isDefault(value: String): Boolean = value.isEmpty
      override def writeWithoutTag(value: String, out: CodedOutputStream): Unit =
        out.writeStringNoTag(value)
    }

  implicit val bytesWriter: PBScalarValueWriter[Array[Byte]] =
    new PBScalarValueWriter[Array[Byte]] {
      override def wireType: Int                          = WIRETYPE_LENGTH_DELIMITED
      override def isDefault(value: Array[Byte]): Boolean = value.isEmpty
      override def writeWithoutTag(value: Array[Byte], out: CodedOutputStream): Unit =
        out.writeByteArrayNoTag(value)
    }

  implicit def keyValuePairWriter[K, V](implicit
      keyWriter: PBScalarValueWriter[K],
      valueWriter: PBScalarValueWriter[V]
  ): PBScalarValueWriter[(K, V)] =
    new PBScalarValueWriter[(K, V)] {
      override def wireType: Int = WIRETYPE_LENGTH_DELIMITED
      override def isDefault(pair: (K, V)): Boolean =
        keyWriter.isDefault(pair._1) && valueWriter.isDefault(pair._2)
      override def writeWithoutTag(pair: (K, V), out: CodedOutputStream): Unit = {
        val buffer    = new ByteArrayOutputStream()
        val bufferOut = CodedOutputStream.newInstance(buffer)
        bufferOut.writeTag(1, keyWriter.wireType)
        keyWriter.writeWithoutTag(pair._1, bufferOut)
        bufferOut.writeTag(2, valueWriter.wireType)
        valueWriter.writeWithoutTag(pair._2, bufferOut)
        bufferOut.flush()
        out.writeByteArrayNoTag(buffer.toByteArray)
      }
    }

  @deprecated("Please use an enumeratum IntEnum instead", since = "0.5.2")
  implicit def enumWriter[E](implicit
      values: Enum.Values[E],
      ordering: Ordering[E]
  ): PBScalarValueWriter[E] =
    new PBScalarValueWriter[E] {
      override def wireType: Int                = WIRETYPE_VARINT
      override def isDefault(value: E): Boolean = Enum.toInt(value) == 0
      override def writeWithoutTag(value: E, out: CodedOutputStream): Unit =
        out.writeInt32NoTag(Enum.toInt(value))
    }

  implicit def enumerationWriter[E <: Enumeration#Value]: PBScalarValueWriter[E] =
    new PBScalarValueWriter[E] {
      override def wireType: Int                = WIRETYPE_VARINT
      override def isDefault(value: E): Boolean = value.id == 0
      override def writeWithoutTag(value: E, out: CodedOutputStream): Unit =
        out.writeInt32NoTag(value.id)
    }

  implicit def enumeratumIntEnumEntryWriter[E <: IntEnumEntry]: PBScalarValueWriter[E] =
    new PBScalarValueWriter[E] {
      override def wireType: Int                = WIRETYPE_VARINT
      override def isDefault(entry: E): Boolean = entry.value == 0
      override def writeWithoutTag(entry: E, out: CodedOutputStream): Unit =
        out.writeInt32NoTag(entry.value)
    }

  implicit def leftWriter[A, B](implicit
      writer: PBScalarValueWriter[A]
  ): PBScalarValueWriter[Left[A, B]] =
    writer.contramap(_.value)

  implicit def rightWriter[A, B](implicit
      writer: PBScalarValueWriter[B]
  ): PBScalarValueWriter[Right[A, B]] =
    writer.contramap(_.value)

}

object PBScalarValueWriter extends PBScalarValueWriterImplicits {
  def apply[A: PBScalarValueWriter]: PBScalarValueWriter[A] = implicitly
}
