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
import java.io.ByteArrayOutputStream

trait PBFieldWriter[A] {

  def writeTo(index: Int, value: A, out: CodedOutputStream, flags: PBFieldWriter.Flags): Unit

}

trait PBFieldWriterImplicits {

  def instance[A](f: (Int, A, CodedOutputStream, PBFieldWriter.Flags) => Unit): PBFieldWriter[A] =
    new PBFieldWriter[A] {
      override def writeTo(
          index: Int,
          value: A,
          out: CodedOutputStream,
          flags: PBFieldWriter.Flags
      ): Unit =
        f(index, value, out, flags)
    }

  implicit def scalarWriter[A](implicit writer: PBScalarValueWriter[A]): PBFieldWriter[A] =
    instance { (index: Int, value: A, out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
      if (flags.skipDefaultValue && writer.isDefault(value)) {
        // skip the field
      } else {
        out.writeTag(index, writer.wireType)
        writer.writeWithoutTag(value, out)
      }
    }

  implicit def optionWriter[A](implicit writer: PBFieldWriter[A]): PBFieldWriter[Option[A]] =
    instance {
      (index: Int, option: Option[A], out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
        option.foreach(v => writer.writeTo(index, v, out, flags))
    }

  implicit def listWriter[A](implicit writer: PBScalarValueWriter[A]): PBFieldWriter[List[A]] =
    instance { (index: Int, list: List[A], out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
      if (writer.canBePacked && !flags.unpacked) {
        if (list.nonEmpty) {
          val buffer    = new ByteArrayOutputStream()
          val packedOut = CodedOutputStream.newInstance(buffer)
          list.foreach(value => writer.writeWithoutTag(value, packedOut))
          packedOut.flush()
          out.writeByteArray(index, buffer.toByteArray)
        }
      } else {
        list.foreach { value =>
          out.writeTag(index, writer.wireType)
          writer.writeWithoutTag(value, out)
        }
      }
    }

  implicit def mapWriter[K, V](implicit
      writer: PBFieldWriter[List[(K, V)]]
  ): PBFieldWriter[Map[K, V]] =
    instance { (index: Int, value: Map[K, V], out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
      writer.writeTo(index, value.toList, out, flags)
    }

  implicit def collectionMapWriter[K, V](implicit
      writer: PBFieldWriter[List[(K, V)]]
  ): PBFieldWriter[collection.Map[K, V]] =
    instance {
      (
          index: Int,
          value: collection.Map[K, V],
          out: CodedOutputStream,
          flags: PBFieldWriter.Flags
      ) => writer.writeTo(index, value.toList, out, flags)
    }

  implicit def seqWriter[A](implicit writer: PBFieldWriter[List[A]]): PBFieldWriter[Seq[A]] =
    instance { (index: Int, value: Seq[A], out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
      writer.writeTo(index, value.toList, out, flags)
    }

}

object PBFieldWriter extends PBFieldWriterImplicits {

  def apply[A: PBFieldWriter]: PBFieldWriter[A] = implicitly

  /**
   * @param skipDefaultValue If true, the field should not be written if it is a scalar field with the default value for its type
   * @param unpacked If true, the field should be written using unpacked encoding if it is a primitive repeated field
   */
  case class Flags(
      skipDefaultValue: Boolean,
      unpacked: Boolean
  )

}
