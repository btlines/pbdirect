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

import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.nat._

trait PBMessageReader[A] {
  def read(bytes: Array[Byte]): A
}

trait PBMessageReaderImplicits {

  def instance[A](f: Array[Byte] => A): PBMessageReader[A] =
    new PBMessageReader[A] {
      override def read(bytes: Array[Byte]): A = f(bytes)
    }

  object collectFieldIndices extends Poly1 {
    implicit def annotatedCase[N <: Nat] =
      at[(Some[pbIndex], N)] {
        case (Some(annotation), _) => FieldIndex(annotation.first :: annotation.more.toList)
      }
    implicit def unannotatedCase[N <: Nat](implicit toInt: ToInt[N]) =
      at[(None.type, N)] {
        case (None, _) => FieldIndex(List(toInt() + 1))
      }
  }

  implicit def prodReader[A, R <: HList, Anns <: HList, ZWI <: HList, I <: HList](implicit
      gen: Generic.Aux[A, R],
      annotations: Annotations.Aux[pbIndex, A, Anns],
      zwi: ZipWithIndex.Aux[Anns, ZWI],
      indices: Mapper.Aux[collectFieldIndices.type, ZWI, I],
      reader: Lazy[PBProductReader[R, I]]
  ): PBMessageReader[A] =
    instance { (bytes: Array[Byte]) =>
      val fieldIndices = annotations.apply.zipWithIndex.map(collectFieldIndices)
      gen.from(reader.value.read(fieldIndices, bytes))
    }

}

object PBMessageReader extends PBMessageReaderImplicits {
  def apply[A: PBMessageReader]: PBMessageReader[A] = implicitly
}
