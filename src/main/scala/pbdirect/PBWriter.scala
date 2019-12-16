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

import cats.{Contravariant, Functor}
import com.google.protobuf.CodedOutputStream
import shapeless._
import shapeless.ops.hlist._
import enumeratum.values.IntEnumEntry

trait PBMessageWriter[A] {
  def writeTo(value: A, out: CodedOutputStream): Unit
}

trait PBFieldWriter[A] {
  def writeTo(index: Int, value: A, out: CodedOutputStream): Unit
}

case class FieldIndex(value: Int)

trait PBMessageWriterImplicits {
  def instance[A](f: (A, CodedOutputStream) => Unit): PBMessageWriter[A] =
    new PBMessageWriter[A] {
      override def writeTo(value: A, out: CodedOutputStream): Unit =
        f(value, out)
    }
  implicit val hnilWriter: PBMessageWriter[HNil] = instance { (_: HNil, _: CodedOutputStream) =>
    ()
  }
  implicit def consWriter[H, T <: HList](
      implicit head: PBFieldWriter[H],
      tail: Lazy[PBMessageWriter[T]]): PBMessageWriter[(FieldIndex, H) :: T] =
    instance { (value: (FieldIndex, H) :: T, out: CodedOutputStream) =>
      head.writeTo(value.head._1.value, value.head._2, out)
      tail.value.writeTo(value.tail, out)
    }

  object zipWithIndex extends Poly2 {
    implicit def defaultCase[T] = at[Some[pbIndex], T] {
      case (Some(annotation), value) => (FieldIndex(annotation.value), value)
    }
  }

  implicit def prodWriter[A, R <: HList, Anns <: HList, Z <: HList](
      implicit gen: Generic.Aux[A, R],
      annotations: Annotations.Aux[pbIndex, A, Anns],
      zw: ZipWith.Aux[Anns, R, zipWithIndex.type, Z],
      writer: Lazy[PBMessageWriter[Z]]): PBMessageWriter[A] =
    instance { (value: A, out: CodedOutputStream) =>
      val fields        = gen.to(value)
      val indexedFields = annotations.apply.zipWith(fields)(zipWithIndex)
      writer.value.writeTo(indexedFields, out)
    }

  implicit val cnilWriter: PBMessageWriter[CNil] = instance { (_: CNil, _: CodedOutputStream) =>
    throw new Exception("Can't write CNil")
  }
  implicit def cconsWriter[H, T <: Coproduct](
      implicit head: PBMessageWriter[H],
      tail: PBMessageWriter[T]): PBMessageWriter[H :+: T] =
    instance { (value: H :+: T, out: CodedOutputStream) =>
      value match {
        case Inl(h) => head.writeTo(h, out)
        case Inr(t) => tail.writeTo(t, out)
      }
    }
  implicit def coprodWriter[A, R <: Coproduct](
      implicit gen: Generic.Aux[A, R],
      writer: PBMessageWriter[R]): PBMessageWriter[A] =
    instance { (value: A, out: CodedOutputStream) =>
      writer.writeTo(gen.to(value), out)
    }

}

trait LowPriorityPBFieldWriterImplicits {
  def instance[A](f: (Int, A, CodedOutputStream) => Unit): PBFieldWriter[A] =
    new PBFieldWriter[A] {
      override def writeTo(index: Int, value: A, out: CodedOutputStream): Unit =
        f(index, value, out)
    }
  implicit def embeddedMessageFieldWriter[A](
      implicit messageWriter: PBMessageWriter[A]): PBFieldWriter[A] =
    instance { (index, message, out) =>
      {
        val buffer    = new ByteArrayOutputStream()
        val bufferOut = CodedOutputStream.newInstance(buffer)
        messageWriter.writeTo(message, bufferOut)
        bufferOut.flush()
        out.writeByteArray(index, buffer.toByteArray)
      }
    }
  implicit def functorWriter[F[_], A](
      implicit functor: Functor[F],
      writer: PBFieldWriter[A]): PBFieldWriter[F[A]] =
    instance { (index: Int, value: F[A], out: CodedOutputStream) =>
      functor.map(value) { v =>
        writer.writeTo(index, v, out)
      }
      ()
    }
}

trait PBFieldWriterImplicits extends LowPriorityPBFieldWriterImplicits {
  implicit object BooleanWriter extends PBFieldWriter[Boolean] {
    override def writeTo(index: Int, value: Boolean, out: CodedOutputStream): Unit =
      out.writeBool(index, value)
  }
  implicit object ByteWriter extends PBFieldWriter[Byte] {
    override def writeTo(index: Int, value: Byte, out: CodedOutputStream): Unit =
      out.writeInt32(index, value.toInt)
  }
  implicit object ShortWriter extends PBFieldWriter[Short] {
    override def writeTo(index: Int, value: Short, out: CodedOutputStream): Unit =
      out.writeInt32(index, value.toInt)
  }
  implicit object IntWriter extends PBFieldWriter[Int] {
    override def writeTo(index: Int, value: Int, out: CodedOutputStream): Unit =
      out.writeInt32(index, value)
  }
  implicit object LongWriter extends PBFieldWriter[Long] {
    override def writeTo(index: Int, value: Long, out: CodedOutputStream): Unit =
      out.writeInt64(index, value)
  }
  implicit object FloatWriter extends PBFieldWriter[Float] {
    override def writeTo(index: Int, value: Float, out: CodedOutputStream): Unit =
      out.writeFloat(index, value)
  }
  implicit object DoubleWriter extends PBFieldWriter[Double] {
    override def writeTo(index: Int, value: Double, out: CodedOutputStream): Unit =
      out.writeDouble(index, value)
  }
  implicit object StringWriter extends PBFieldWriter[String] {
    override def writeTo(index: Int, value: String, out: CodedOutputStream): Unit =
      out.writeString(index, value)
  }
  implicit object BytesWriter extends PBFieldWriter[Array[Byte]] {
    override def writeTo(index: Int, value: Array[Byte], out: CodedOutputStream): Unit =
      out.writeByteArray(index, value)
  }
  implicit def optionWriter[A](implicit writer: PBFieldWriter[A]): PBFieldWriter[Option[A]] =
    instance { (index: Int, option: Option[A], out: CodedOutputStream) =>
      option.foreach(v => writer.writeTo(index, v, out))
    }
  implicit def listWriter[A](implicit writer: PBFieldWriter[A]): PBFieldWriter[List[A]] =
    instance { (index: Int, list: List[A], out: CodedOutputStream) =>
      list.foreach(v => writer.writeTo(index, v, out))
    }
  implicit def keyValuePairWriter[K, V](
      implicit keyWriter: PBFieldWriter[K],
      valueWriter: PBFieldWriter[V]): PBFieldWriter[(K, V)] =
    instance { (index: Int, pair: (K, V), out: CodedOutputStream) =>
      val buffer    = new ByteArrayOutputStream()
      val bufferOut = CodedOutputStream.newInstance(buffer)
      keyWriter.writeTo(1, pair._1, bufferOut)
      valueWriter.writeTo(2, pair._2, bufferOut)
      bufferOut.flush()
      out.writeByteArray(index, buffer.toByteArray)
    }
  implicit def mapWriter[K, V](
      implicit writer: PBFieldWriter[List[(K, V)]]): PBFieldWriter[Map[K, V]] =
    instance { (index: Int, value: Map[K, V], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }
  implicit def collectionMapWriter[K, V](
      implicit writer: PBFieldWriter[List[(K, V)]]): PBFieldWriter[collection.Map[K, V]] =
    instance { (index: Int, value: collection.Map[K, V], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }
  implicit def seqWriter[A](implicit writer: PBFieldWriter[List[A]]): PBFieldWriter[Seq[A]] =
    instance { (index: Int, value: Seq[A], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }
  implicit def enumWriter[E](
      implicit values: Enum.Values[E],
      ordering: Ordering[E]): PBFieldWriter[E] =
    instance { (index: Int, value: E, out: CodedOutputStream) =>
      out.writeInt32(index, Enum.toInt(value))
    }
  implicit def enumerationWriter[E <: Enumeration#Value]: PBFieldWriter[E] =
    instance { (index: Int, value: E, out: CodedOutputStream) =>
      out.writeInt32(index, value.id)
    }
  implicit def enumeratumIntEnumEntryWriter[E <: IntEnumEntry]: PBFieldWriter[E] =
    instance { (index: Int, entry: E, out: CodedOutputStream) =>
      out.writeInt32(index, entry.value)
    }

  implicit object ContravariantWriter extends Contravariant[PBFieldWriter] {
    override def contramap[A, B](writer: PBFieldWriter[A])(f: B => A) =
      instance { (index: Int, b: B, out: CodedOutputStream) =>
        writer.writeTo(index, f(b), out)
      }
  }
}

object PBMessageWriter extends PBMessageWriterImplicits {
  def apply[A: PBMessageWriter]: PBMessageWriter[A] = implicitly
}

object PBFieldWriter extends PBFieldWriterImplicits {
  def apply[A: PBFieldWriter]: PBFieldWriter[A] = implicitly
}
