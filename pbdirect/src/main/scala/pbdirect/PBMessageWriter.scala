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

import com.google.protobuf.CodedOutputStream
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.nat._

trait PBMessageWriter[A] {
  def writeTo(value: A, out: CodedOutputStream): Unit
}

trait PBMessageWriterImplicits {

  def instance[A](f: (A, CodedOutputStream) => Unit): PBMessageWriter[A] =
    new PBMessageWriter[A] {
      override def writeTo(value: A, out: CodedOutputStream): Unit =
        f(value, out)
    }

  object zipWithFieldIndex extends Poly2 {
    implicit def annotatedCase[T, N <: Nat] =
      at[Some[pbIndex], (T, N)] {
        case (Some(annotation), (value, _)) =>
          (FieldIndex(annotation.first :: annotation.more.toList), value)
      }
    implicit def unannotatedCase[T, N <: Nat](implicit toInt: ToInt[N]) =
      at[None.type, (T, N)] {
        case (None, (value, _)) =>
          (FieldIndex(List(toInt() + 1)), value)
      }
  }

  object zipWithModifiers extends Poly2 {
    implicit def annotatedCase[A] =
      at[(FieldIndex, A), Some[pbUnpacked]] {
        case ((fieldIndex, value), Some(_)) =>
          (fieldIndex, value, FieldModifiers(unpacked = true))
      }
    implicit def unannotatedCase[A] =
      at[(FieldIndex, A), None.type] {
        case ((fieldIndex, value), None) =>
          (fieldIndex, value, FieldModifiers(unpacked = false))
      }
  }

  implicit def prodWriter[
      A,
      R <: HList,
      IA <: HList,
      UA <: HList,
      ZWI <: HList,
      ZWFI <: HList,
      ZWM <: HList
  ](implicit
      gen: Generic.Aux[A, R],
      indexAnns: Annotations.Aux[pbIndex, A, IA],
      unpackedAnns: Annotations.Aux[pbUnpacked, A, UA],
      zwi: ZipWithIndex.Aux[R, ZWI],
      zwfi: ZipWith.Aux[IA, ZWI, zipWithFieldIndex.type, ZWFI],
      zwm: ZipWith.Aux[ZWFI, UA, zipWithModifiers.type, ZWM],
      writer: Lazy[PBProductWriter[ZWM]]
  ): PBMessageWriter[A] =
    instance { (value: A, out: CodedOutputStream) =>
      val fields                     = gen.to(value)
      val fieldsWithIndices          = fields.zipWithIndex
      val indexedFields              = indexAnns.apply.zipWith(fieldsWithIndices)(zipWithFieldIndex)
      val indexedFieldsWithModifiers = indexedFields.zipWith(unpackedAnns.apply)(zipWithModifiers)
      writer.value.writeTo(indexedFieldsWithModifiers, out)
    }

}

object PBMessageWriter extends PBMessageWriterImplicits {
  def apply[A: PBMessageWriter]: PBMessageWriter[A] = implicitly
}
