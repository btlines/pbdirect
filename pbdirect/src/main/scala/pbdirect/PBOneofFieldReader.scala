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

trait PBOneofFieldReader[A <: Coproduct] {
  def read(indices: List[Int], bytes: Array[Byte]): Option[A]
}

trait PBOneofFieldReaderImplicits {
  def instance[A <: Coproduct](f: (List[Int], Array[Byte]) => Option[A]): PBOneofFieldReader[A] =
    new PBOneofFieldReader[A] {
      override def read(indices: List[Int], bytes: Array[Byte]): Option[A] = f(indices, bytes)
    }

  implicit val cnilReader: PBOneofFieldReader[CNil] = instance((_, _) => None)

  implicit def cconsReader[H, T <: Coproduct](implicit
      headReader: PBFieldReader[Option[H]],
      tailReader: Lazy[PBOneofFieldReader[T]]
  ): PBOneofFieldReader[H :+: T] =
    instance { (indices: List[Int], bytes: Array[Byte]) =>
      headReader
        .read(indices.head, bytes)
        .map(Inl(_))
        .orElse(tailReader.value.read(indices.tail, bytes).map(Inr(_)))
    }

}

object PBOneofFieldReader extends PBOneofFieldReaderImplicits {
  def apply[A <: Coproduct: PBOneofFieldReader]: PBOneofFieldReader[A] = implicitly
}
