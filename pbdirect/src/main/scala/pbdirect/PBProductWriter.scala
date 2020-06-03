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

trait PBProductWriter[A <: HList] {
  def writeTo(indexedValues: A, out: CodedOutputStream): Unit
}

trait PBProductWriterImplicits {
  def instance[A <: HList](f: (A, CodedOutputStream) => Unit): PBProductWriter[A] =
    new PBProductWriter[A] {
      override def writeTo(indexedValues: A, out: CodedOutputStream): Unit =
        f(indexedValues, out)
    }

  implicit val hnilWriter: PBProductWriter[HNil] = instance { (_: HNil, _: CodedOutputStream) =>
    ()
  }

  implicit def hconsWriter[H, T <: HList](implicit
      head: PBFieldWriter[H],
      tail: Lazy[PBProductWriter[T]]
  ): PBProductWriter[(FieldIndex, H, FieldModifiers) :: T] =
    instance { (indexedValues: (FieldIndex, H, FieldModifiers) :: T, out: CodedOutputStream) =>
      val headIndex     = indexedValues.head._1.values.head
      val headValue     = indexedValues.head._2
      val headModifiers = indexedValues.head._3
      val flags         = PBFieldWriter.Flags(skipDefaultValue = true, unpacked = headModifiers.unpacked)
      head.writeTo(headIndex, headValue, out, flags)
      tail.value.writeTo(indexedValues.tail, out)
    }

  implicit def hconsCoproductOneofWriter[H <: Coproduct, T <: HList](implicit
      head: PBOneofFieldWriter[H],
      tail: Lazy[PBProductWriter[T]]
  ): PBProductWriter[(FieldIndex, Option[H], FieldModifiers) :: T] =
    instance {
      (indexedValues: (FieldIndex, Option[H], FieldModifiers) :: T, out: CodedOutputStream) =>
        indexedValues.head match {
          case (headFieldIndex, Some(headValue), headModifiers) =>
            val flags =
              PBFieldWriter.Flags(skipDefaultValue = true, unpacked = headModifiers.unpacked)
            head.writeTo(headFieldIndex.values, headValue, out, flags)
          case _ => // skip writing the field
        }
        tail.value.writeTo(indexedValues.tail, out)
    }

  // write an Either[A, B] by treating it as a Coproduct (Left[A, B] :+: Right[A, B] :+: CNil)
  implicit def hconsEitherOneofWriter[A, B, H <: Coproduct, T <: HList](implicit
      gen: Generic.Aux[Either[A, B], H],
      productWriter: PBProductWriter[(FieldIndex, Option[H], FieldModifiers) :: T]
  ): PBProductWriter[(FieldIndex, Option[Either[A, B]], FieldModifiers) :: T] =
    instance {
      (
          indexedValues: (FieldIndex, Option[Either[A, B]], FieldModifiers) :: T,
          out: CodedOutputStream
      ) =>
        val headEitherAsCoproduct: Option[H] = indexedValues.head._2.map(gen.to)
        val head                             = indexedValues.head.copy(_2 = headEitherAsCoproduct)
        productWriter.writeTo(head :: indexedValues.tail, out)
    }

}

object PBProductWriter extends PBProductWriterImplicits {
  def apply[A <: HList: PBProductWriter]: PBProductWriter[A] = implicitly
}
