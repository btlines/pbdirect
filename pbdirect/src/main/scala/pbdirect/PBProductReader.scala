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

trait PBProductReader[R <: HList, I <: HList] {
  def read(indices: I, bytes: Array[Byte]): R
}

trait PBProductReaderImplicits {

  def instance[R <: HList, I <: HList](f: (I, Array[Byte]) => R): PBProductReader[R, I] =
    new PBProductReader[R, I] {
      def read(indices: I, bytes: Array[Byte]): R = f(indices, bytes)
    }

  implicit val hnilProductReader: PBProductReader[HNil, HNil] = PBProductReader.instance {
    (_: HNil, _: Array[Byte]) => HNil
  }

  implicit def hconsProductReader[H, T <: HList, IT <: HList](implicit
      head: PBFieldReader[H],
      tail: Lazy[PBProductReader[T, IT]]
  ): PBProductReader[H :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      head.read(indices.head.values.head, bytes) :: tail.value.read(indices.tail, bytes)
    }

  implicit def hconsCoproductOneofProductReader[H <: Coproduct, T <: HList, IT <: HList](implicit
      head: PBOneofFieldReader[H],
      tail: Lazy[PBProductReader[T, IT]]
  ): PBProductReader[Option[H] :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      head.read(indices.head.values, bytes) :: tail.value.read(indices.tail, bytes)
    }

  // read an Either[A, B] by treating it as a Coproduct (Left[A, B] :+: Right[A, B] :+: CNil)
  implicit def hconsEitherOneofProductReader[A, B, H <: Coproduct, T <: HList, IT <: HList](implicit
      gen: Generic.Aux[Either[A, B], H],
      productReader: PBProductReader[Option[H] :: T, FieldIndex :: IT]
  ): PBProductReader[Option[Either[A, B]] :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      val result = productReader.read(indices, bytes)
      result.head.map(gen.from) :: result.tail
    }

}

object PBProductReader extends PBProductReaderImplicits {
  def apply[R <: HList, I <: HList](implicit reader: PBProductReader[R, I]) = reader
}
