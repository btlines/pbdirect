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

trait PBOneofFieldWriter[A <: Coproduct] {
  def writeTo(
      indices: List[Int],
      value: A,
      out: CodedOutputStream,
      flags: PBFieldWriter.Flags
  ): Unit
}

trait PBOneofFieldWriterImplicits {

  def instance[A <: Coproduct](
      f: (List[Int], A, CodedOutputStream, PBFieldWriter.Flags) => Unit
  ): PBOneofFieldWriter[A] =
    new PBOneofFieldWriter[A] {
      override def writeTo(
          indices: List[Int],
          value: A,
          out: CodedOutputStream,
          flags: PBFieldWriter.Flags
      ): Unit =
        f(indices, value, out, flags)
    }

  implicit val cnilWriter: PBOneofFieldWriter[CNil] =
    instance((_, _, _, _) => throw new IllegalStateException("Cannot write CNil"))

  implicit def cconsWriter[H, T <: Coproduct](implicit
      headWriter: PBFieldWriter[H],
      tailWriter: Lazy[PBOneofFieldWriter[T]]
  ): PBOneofFieldWriter[H :+: T] =
    instance {
      (indices: List[Int], value: H :+: T, out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
        value match {
          case Inl(h) =>
            // We should always write a oneof field, even if it's the default value,
            // so the reader knows which field was set
            val updatedFlags = flags.copy(skipDefaultValue = false)
            headWriter.writeTo(indices.head, h, out, updatedFlags)
          case Inr(t) =>
            tailWriter.value.writeTo(indices.tail, t, out, flags)
        }
    }

}

object PBOneofFieldWriter extends PBOneofFieldWriterImplicits {
  def apply[A <: Coproduct: PBOneofFieldWriter]: PBOneofFieldWriter[A] = implicitly
}
