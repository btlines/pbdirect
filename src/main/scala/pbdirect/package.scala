import java.io.ByteArrayOutputStream

import cats.data.{NonEmptyList => NEL}
import com.google.protobuf.{ CodedInputStream, CodedOutputStream }

package object pbdirect {
  implicit class PBWriterOps[A <: AnyRef](private val a: A) extends AnyVal {
    def toPB(implicit writer: PBWriter[A]): Array[Byte] = {
      val out = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(out)
      writer.writeTo(NEL.one(1), a, pbOut)
      pbOut.flush()
      val bytes = out.toByteArray
      // remove the tag and return the content
      val input = CodedInputStream.newInstance(bytes)
      input.readTag()
      input.readByteArray()
    }
  }
  implicit class PBParserOps(private val bytes: Array[Byte]) extends AnyVal {
    def pbTo[A](implicit reader: PBParser[A]): A = {
      // wraps the bytes into a protobuf single field message
      val out = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(out)
      pbOut.writeByteArray(1, bytes)
      pbOut.flush()
      reader.parse(NEL.one(1), out.toByteArray)
    }
  }
}
