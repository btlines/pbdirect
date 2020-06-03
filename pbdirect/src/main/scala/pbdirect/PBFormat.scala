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
import cats.{Contravariant, Invariant}
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

sealed trait PBFormat[A] extends PBScalarValueReader[A] with PBScalarValueWriter[A]

trait PBFormatImplicits {
  implicit object InvariantFormat extends Invariant[PBFormat] {
    override def imap[A, B](format: PBFormat[A])(f: A => B)(g: B => A): PBFormat[B] =
      PBFormat[B](
        Functor[PBScalarValueReader].map(format)(f),
        Contravariant[PBScalarValueWriter].contramap(format)(g)
      )
  }
}

object PBFormat extends PBFormatImplicits {
  def apply[A](implicit
      reader: PBScalarValueReader[A],
      writer: PBScalarValueWriter[A]
  ): PBFormat[A] =
    new PBFormat[A] {
      override def read(input: CodedInputStream): A = reader.read(input)
      override def wireType: Int                    = writer.wireType
      override def isDefault(value: A): Boolean     = writer.isDefault(value)
      override def defaultValue: A                  = reader.defaultValue
      override def canBePacked: Boolean             = reader.canBePacked
      override def writeWithoutTag(value: A, out: CodedOutputStream): Unit =
        writer.writeWithoutTag(value, out)
    }
}
