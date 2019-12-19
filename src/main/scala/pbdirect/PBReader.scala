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

import java.io.ByteArrayOutputStream

import cats.Functor
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.nat._
import enumeratum.values.{IntEnum, IntEnumEntry}

import scala.util.Try

trait PBReader[A] {
  def read(input: CodedInputStream): A
}

trait PBProductReader[R <: HList, I <: HList] {
  def read(indices: I, bytes: Array[Byte]): R
}
object PBProductReader {
  def instance[R <: HList, I <: HList](f: (I, Array[Byte]) => R): PBProductReader[R, I] =
    new PBProductReader[R, I] {
      def read(indices: I, bytes: Array[Byte]): R = f(indices, bytes)
    }
  implicit val hnilProductReader: PBProductReader[HNil, HNil] = PBProductReader.instance {
    (indices: HNil, bytes: Array[Byte]) =>
      HNil
  }
  implicit def consProductReader[H, T <: HList, IT <: HList](
      implicit
      headParser: PBParser[H],
      tail: Lazy[PBProductReader[T, IT]]): PBProductReader[H :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      headParser.parse(indices.head.values.head, bytes) :: tail.value.read(indices.tail, bytes)
    }

}

trait LowerPriorityPBReaderImplicits {
  def instance[A](f: CodedInputStream => A): PBReader[A] =
    new PBReader[A] {
      override def read(input: CodedInputStream): A = f(input)
    }
  implicit def coprodReader[A, R <: Coproduct](
      implicit
      gen: Generic.Aux[A, R],
      repr: Lazy[PBParser[R]]): PBReader[A] = instance { (input: CodedInputStream) =>
    val bytes = input.readByteArray()

    // wraps the bytes into a protobuf single field message
    val out   = new ByteArrayOutputStream()
    val pbOut = CodedOutputStream.newInstance(out)
    pbOut.writeByteArray(1, bytes)
    pbOut.flush()

    gen.from(repr.value.parse(1, out.toByteArray))
  }
}

trait PBReaderImplicits extends LowerPriorityPBReaderImplicits {

  object collectFieldIndices extends Poly1 {
    implicit def annotatedCase[N <: Nat] = at[(Some[pbIndex], N)] {
      case (Some(annotation), _) => FieldIndex(annotation.first :: annotation.more.toList)
    }
    implicit def unannotatedCase[N <: Nat](implicit toInt: ToInt[N]) = at[(None.type, N)] {
      case (None, n) => FieldIndex(List(toInt() + 1))
    }
  }

  implicit def prodReader[A, R <: HList, Anns <: HList, ZWI <: HList, I <: HList](
      implicit
      gen: Generic.Aux[A, R],
      annotations: Annotations.Aux[pbIndex, A, Anns],
      zwi: ZipWithIndex.Aux[Anns, ZWI],
      indices: Mapper.Aux[collectFieldIndices.type, ZWI, I],
      reader: Lazy[PBProductReader[R, I]]): PBReader[A] = instance { (input: CodedInputStream) =>
    val fieldIndices = annotations.apply.zipWithIndex.map(collectFieldIndices)
    val bytes        = input.readByteArray()
    gen.from(reader.value.read(fieldIndices, bytes))
  }

  implicit def enumReader[A](
      implicit
      values: Enum.Values[A],
      ordering: Ordering[A],
      reader: PBReader[Int]): PBReader[A] = instance { (input: CodedInputStream) =>
    Enum.fromInt[A](reader.read(input))
  }
  implicit def enumerationReader[E <: Enumeration](
      implicit
      reader: PBReader[Int],
      gen: Generic.Aux[E, HNil]): PBReader[E#Value] = instance { (input: CodedInputStream) =>
    val enum = gen.from(HNil)
    enum(reader.read(input))
  }
  implicit def enumeratumIntEnumEntryReader[E <: IntEnumEntry](
      implicit
      reader: PBReader[Int],
      enum: IntEnum[E]): PBReader[E] = instance { (input: CodedInputStream) =>
    enum.withValue(reader.read(input))
  }
  implicit def keyValuePairReader[K, V](
      implicit keyParser: PBParser[K],
      valueParser: PBParser[V]): PBReader[(K, V)] = instance { (input: CodedInputStream) =>
    val bytes = input.readByteArray()
    val key   = keyParser.parse(1, bytes)
    val value = valueParser.parse(2, bytes)
    (key, value)
  }
}
object PBReader extends PBReaderImplicits {
  implicit object BooleanReader$ extends PBReader[Boolean] {
    override def read(input: CodedInputStream): Boolean = input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ByteReader$ extends PBReader[Byte] {
    override def read(input: CodedInputStream): Byte = input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ShortReader$ extends PBReader[Short] {
    override def read(input: CodedInputStream): Short = input.readInt32().toShort
  }
  implicit object IntReader$ extends PBReader[Int] {
    override def read(input: CodedInputStream): Int = input.readInt32()
  }
  implicit object LongReader$ extends PBReader[Long] {
    override def read(input: CodedInputStream): Long = input.readInt64()
  }
  implicit object FloatReader$ extends PBReader[Float] {
    override def read(input: CodedInputStream): Float = input.readFloat()
  }
  implicit object DoubleReader$ extends PBReader[Double] {
    override def read(input: CodedInputStream): Double = input.readDouble()
  }
  implicit object StringReader$ extends PBReader[String] {
    override def read(input: CodedInputStream): String = input.readString()
  }
  implicit object BytesReader$ extends PBReader[Array[Byte]] {
    override def read(input: CodedInputStream): Array[Byte] = input.readByteArray()
  }

  def apply[A: PBReader]: PBReader[A] = implicitly

  implicit object FunctorReader extends Functor[PBReader] {
    override def map[A, B](reader: PBReader[A])(f: A => B): PBReader[B] = instance {
      (input: CodedInputStream) =>
        f(reader.read(input))
    }
  }
}

trait PBParser[A] {
  def parse(index: Int, bytes: Array[Byte]): A
}

trait LowPriorityPBParserImplicits {
  def instance[A](f: (Int, Array[Byte]) => A): PBParser[A] = new PBParser[A] {
    override def parse(index: Int, bytes: Array[Byte]): A = f(index, bytes)
  }

  implicit val cnilParser: PBParser[CNil] = instance { (index: Int, bytes: Array[Byte]) =>
    throw new UnsupportedOperationException("Can't read CNil")
  }
  implicit def cconsParser[H, T <: Coproduct](
      implicit
      head: PBParser[H],
      tail: Lazy[PBParser[T]]): PBParser[H :+: T] = instance { (index: Int, bytes: Array[Byte]) =>
    Try {
      Inl(head.parse(index, bytes))
    } getOrElse {
      Inr(tail.value.parse(index, bytes))
    }
  }
}

trait PBParserImplicits extends LowPriorityPBParserImplicits {
  implicit def repeatedParser[A](implicit reader: PBReader[A]): PBParser[List[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                          => done = true
          case tag if (tag >> 3) == index => as ::= reader.read(input)
          case tag                        => input.skipField(tag)
        }
      }
      as.reverse
    }
  implicit def requiredParser[A](implicit reader: PBReader[A]): PBParser[A] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                          => done = true
          case tag if (tag >> 3) == index => as ::= reader.read(input)
          case tag                        => input.skipField(tag)
        }
      }
      as.head
    }
  implicit def optionalParser[A](implicit parser: PBParser[List[A]]): PBParser[Option[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).lastOption
    }
  implicit def mapParser[K, V](implicit parser: PBParser[List[(K, V)]]): PBParser[Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).toMap
    }
  implicit def collectionMapParser[K, V](
      implicit parser: PBParser[List[(K, V)]]): PBParser[collection.Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).toMap
    }
  implicit def seqParser[A](implicit parser: PBParser[List[A]]): PBParser[Seq[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes)
    }
}

object PBParser extends PBParserImplicits
