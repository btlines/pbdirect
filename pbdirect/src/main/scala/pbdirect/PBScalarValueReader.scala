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

import cats.Functor
import cats.syntax.functor._
import com.google.protobuf.CodedInputStream
import shapeless._
import shapeless.tag.@@
import enumeratum.values.{IntEnum, IntEnumEntry}

trait PBScalarValueReader[A] {

  /**
   * The default value to use as a fallback if the field is missing from the message.
   */
  def defaultValue: A

  /**
   * Whether it's possible for a repeated field of this type to be encoded using the packed encoding.
   * This is only possible for "primitive" protobuf types:
   * int{32,64}, {u,s}int{32,64}, bool, enum, fixed32, sfixed32, float, fixed64, sfixed64, double
   */
  def canBePacked: Boolean

  /**
   * Read a value of type A from the current point in the input stream.
   */
  def read(input: CodedInputStream): A
}

trait PBScalarValueReaderImplicits_1 {

  implicit def embeddedMessageReader[A](implicit
      reader: PBMessageReader[A]
  ): PBScalarValueReader[A] =
    new PBScalarValueReader[A] {
      // To construct a default instance of the message
      // we decode a byte array of length zero,
      // i.e. with all of the message's fields missing
      val defaultValue: A      = reader.read(Array[Byte]())
      def canBePacked: Boolean = false
      def read(input: CodedInputStream): A = {
        val bytes = input.readByteArray()
        reader.read(bytes)
      }
    }

}

trait PBScalarValueReaderImplicits extends PBScalarValueReaderImplicits_1 {

  implicit object FunctorReader extends Functor[PBScalarValueReader] {
    override def map[A, B](reader: PBScalarValueReader[A])(f: A => B): PBScalarValueReader[B] =
      new PBScalarValueReader[B] {
        val defaultValue: B                  = f(reader.defaultValue)
        def canBePacked: Boolean             = reader.canBePacked
        def read(input: CodedInputStream): B = f(reader.read(input))
      }
  }

  implicit val booleanReader: PBScalarValueReader[Boolean] = new PBScalarValueReader[Boolean] {
    override def defaultValue: Boolean                  = false
    override def canBePacked: Boolean                   = true
    override def read(input: CodedInputStream): Boolean = input.readBool()
  }

  implicit val untaggedIntReader: PBScalarValueReader[Int] = new PBScalarValueReader[Int] {
    override def defaultValue: Int                  = 0
    override def canBePacked: Boolean               = true
    override def read(input: CodedInputStream): Int = input.readInt32()
  }

  def taggedIntReader[T](readInt: CodedInputStream => Int): PBScalarValueReader[Int @@ T] =
    new PBScalarValueReader[Int @@ T] {
      override def defaultValue: Int @@ T                  = tag[T](0)
      override def canBePacked: Boolean                    = true
      override def read(input: CodedInputStream): Int @@ T = tag[T](readInt(input))
    }

  implicit val unsignedIntReader: PBScalarValueReader[Int @@ Unsigned] =
    taggedIntReader[Unsigned](_.readUInt32())

  implicit val signedIntReader: PBScalarValueReader[Int @@ Signed] =
    taggedIntReader[Signed](_.readSInt32())

  implicit val fixedIntReader: PBScalarValueReader[Int @@ Fixed] =
    taggedIntReader[Fixed](_.readFixed32())

  implicit val fixedSignedIntReader: PBScalarValueReader[Int @@ (Signed with Fixed)] =
    taggedIntReader[Signed with Fixed](_.readSFixed32())

  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit val byteReader: PBScalarValueReader[Byte] = untaggedIntReader.map(_.toByte)

  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit val shortReader: PBScalarValueReader[Short] = untaggedIntReader.map(_.toShort)

  implicit val untaggedLongReader: PBScalarValueReader[Long] = new PBScalarValueReader[Long] {
    override def defaultValue: Long                  = 0L
    override def canBePacked: Boolean                = true
    override def read(input: CodedInputStream): Long = input.readInt64()
  }

  def taggedLongReader[T](readLong: CodedInputStream => Long): PBScalarValueReader[Long @@ T] =
    new PBScalarValueReader[Long @@ T] {
      override def defaultValue: Long @@ T                  = tag[T](0L)
      override def canBePacked: Boolean                     = true
      override def read(input: CodedInputStream): Long @@ T = tag[T](readLong(input))
    }

  implicit val unsignedLongReader: PBScalarValueReader[Long @@ Unsigned] =
    taggedLongReader[Unsigned](_.readUInt64())

  implicit val signedLongReader: PBScalarValueReader[Long @@ Signed] =
    taggedLongReader[Signed](_.readSInt64())

  implicit val fixedLongReader: PBScalarValueReader[Long @@ Fixed] =
    taggedLongReader[Fixed](_.readFixed64())

  implicit val fixedSignedLongReader: PBScalarValueReader[Long @@ (Signed with Fixed)] =
    taggedLongReader[Signed with Fixed](_.readSFixed64())

  implicit val floatReader: PBScalarValueReader[Float] = new PBScalarValueReader[Float] {
    override def defaultValue: Float                  = 0.0f
    override def canBePacked: Boolean                 = true
    override def read(input: CodedInputStream): Float = input.readFloat()
  }

  implicit val doubleReader: PBScalarValueReader[Double] = new PBScalarValueReader[Double] {
    override def defaultValue: Double                  = 0.0
    override def canBePacked: Boolean                  = true
    override def read(input: CodedInputStream): Double = input.readDouble()
  }

  implicit val stringReader: PBScalarValueReader[String] = new PBScalarValueReader[String] {
    override def defaultValue: String                  = ""
    override def canBePacked: Boolean                  = false
    override def read(input: CodedInputStream): String = input.readString()
  }

  implicit val bytesReader: PBScalarValueReader[Array[Byte]] =
    new PBScalarValueReader[Array[Byte]] {
      override def defaultValue: Array[Byte]                  = Array()
      override def canBePacked: Boolean                       = false
      override def read(input: CodedInputStream): Array[Byte] = input.readByteArray()
    }

  @deprecated("Please use an enumeratum IntEnum instead", since = "0.5.2")
  implicit def enumReader[A](implicit
      values: Enum.Values[A],
      ordering: Ordering[A],
      reader: PBScalarValueReader[Int]
  ): PBScalarValueReader[A] =
    new PBScalarValueReader[A] {
      val defaultValue: A                  = Enum.fromInt[A](0)
      def canBePacked: Boolean             = true
      def read(input: CodedInputStream): A = Enum.fromInt[A](reader.read(input))
    }

  implicit def enumerationReader[E <: Enumeration](implicit
      reader: PBScalarValueReader[Int],
      gen: Generic.Aux[E, HNil]
  ): PBScalarValueReader[E#Value] = {
    val enum = gen.from(HNil)
    new PBScalarValueReader[E#Value] {
      val defaultValue: E#Value                  = enum(0)
      def canBePacked: Boolean                   = true
      def read(input: CodedInputStream): E#Value = enum(reader.read(input))
    }
  }

  implicit def enumeratumIntEnumEntryReader[E <: IntEnumEntry](implicit
      reader: PBScalarValueReader[Int],
      enum: IntEnum[E]
  ): PBScalarValueReader[E] =
    new PBScalarValueReader[E] {
      val defaultValue: E                  = enum.withValue(0)
      def canBePacked: Boolean             = true
      def read(input: CodedInputStream): E = enum.withValue(reader.read(input))
    }

  implicit def keyValuePairReader[K, V](implicit
      keyReader: PBScalarValueReader[K],
      valueReader: PBScalarValueReader[V]
  ): PBScalarValueReader[(K, V)] = {
    val defaultKey   = keyReader.defaultValue
    val defaultValue = valueReader.defaultValue
    val defaultPair  = (defaultKey, defaultValue)
    new PBScalarValueReader[(K, V)] {
      val defaultValue: (K, V) = defaultPair
      def canBePacked: Boolean = false
      def read(input: CodedInputStream): (K, V) = {
        val bytes = input.readByteArray()
        val in    = CodedInputStream.newInstance(bytes)
        in.readTag()
        val key = keyReader.read(in)
        in.readTag()
        val value = valueReader.read(in)
        (key, value)
      }
    }
  }

  implicit def leftReader[A, B](implicit
      reader: PBScalarValueReader[A]
  ): PBScalarValueReader[Left[A, B]] =
    reader.map(Left(_))

  implicit def rightReader[A, B](implicit
      reader: PBScalarValueReader[B]
  ): PBScalarValueReader[Right[A, B]] =
    reader.map(Right(_))

}

object PBScalarValueReader extends PBScalarValueReaderImplicits {
  def apply[A: PBScalarValueReader]: PBScalarValueReader[A] = implicitly
}
