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

import com.google.protobuf.CodedInputStream
import com.google.protobuf.WireFormat.{getTagFieldNumber, getTagWireType, WIRETYPE_LENGTH_DELIMITED}

trait PBFieldReader[A] {
  def read(index: Int, bytes: Array[Byte]): A
}

trait PBFieldReaderImplicits {
  def instance[A](f: (Int, Array[Byte]) => A): PBFieldReader[A] =
    new PBFieldReader[A] {
      override def read(index: Int, bytes: Array[Byte]): A = f(index, bytes)
    }

  implicit def repeatedFieldReader[A](implicit
      reader: PBScalarValueReader[A]
  ): PBFieldReader[List[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0 => done = true
          case tag
              if getTagFieldNumber(tag) == index && getTagWireType(
                tag
              ) == WIRETYPE_LENGTH_DELIMITED && reader.canBePacked =>
            // read a packed repeated field
            val packedBytes = input.readByteArray()
            val packedInput = CodedInputStream.newInstance(packedBytes)
            while (!packedInput.isAtEnd())
              as ::= reader.read(packedInput)
          case tag if getTagFieldNumber(tag) == index => as ::= reader.read(input)
          case tag                                    => input.skipField(tag)
        }
      }
      as.reverse
    }

  implicit def requiredFieldReader[A](implicit reader: PBScalarValueReader[A]): PBFieldReader[A] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                                      => done = true
          case tag if getTagFieldNumber(tag) == index => as ::= reader.read(input)
          case tag                                    => input.skipField(tag)
        }
      }
      as.headOption.getOrElse(reader.defaultValue)
    }

  implicit def optionalFieldReader[A](implicit
      reader: PBFieldReader[List[A]]
  ): PBFieldReader[Option[A]] =
    instance((index: Int, bytes: Array[Byte]) => reader.read(index, bytes).lastOption)

  implicit def mapFieldReader[K, V](implicit
      reader: PBFieldReader[List[(K, V)]]
  ): PBFieldReader[Map[K, V]] =
    instance((index: Int, bytes: Array[Byte]) => reader.read(index, bytes).toMap)

  implicit def collectionMapFieldReader[K, V](implicit
      reader: PBFieldReader[List[(K, V)]]
  ): PBFieldReader[collection.Map[K, V]] =
    instance((index: Int, bytes: Array[Byte]) => reader.read(index, bytes).toMap)

  implicit def seqFieldReader[A](implicit reader: PBFieldReader[List[A]]): PBFieldReader[Seq[A]] =
    instance((index: Int, bytes: Array[Byte]) => reader.read(index, bytes))

}

object PBFieldReader extends PBFieldReaderImplicits {
  def apply[A: PBFieldReader]: PBFieldReader[A] = implicitly
}
