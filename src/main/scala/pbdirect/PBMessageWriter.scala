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

import com.google.protobuf.CodedOutputStream
import shapeless._
import shapeless.ops.hlist._

trait PBMessageWriter[A] {
  def writeTo(value: A, out: CodedOutputStream): Unit
}

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
      val headIndex = value.head._1.values.head
      val headValue = value.head._2
      head.writeTo(headIndex, headValue, out)
      tail.value.writeTo(value.tail, out)
    }

  object zipWithFieldIndex extends Poly2 {
    implicit def defaultCase[T] = at[Some[pbIndex], T] {
      case (Some(annotation), value) =>
        (FieldIndex(annotation.first :: annotation.more.toList), value)
    }
  }

  implicit def prodWriter[A, R <: HList, Anns <: HList, Z <: HList](
      implicit gen: Generic.Aux[A, R],
      annotations: Annotations.Aux[pbIndex, A, Anns],
      zw: ZipWith.Aux[Anns, R, zipWithFieldIndex.type, Z],
      writer: Lazy[PBMessageWriter[Z]]): PBMessageWriter[A] =
    instance { (value: A, out: CodedOutputStream) =>
      val fields        = gen.to(value)
      val indexedFields = annotations.apply.zipWith(fields)(zipWithFieldIndex)
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

object PBMessageWriter extends PBMessageWriterImplicits {
  def apply[A: PBMessageWriter]: PBMessageWriter[A] = implicitly
}
