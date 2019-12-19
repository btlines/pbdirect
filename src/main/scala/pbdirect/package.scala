import java.io.ByteArrayOutputStream

import com.google.protobuf.CodedOutputStream

package object pbdirect {
  implicit class PBWriterOps[A](private val a: A) extends AnyVal {
    def toPB(implicit writer: PBMessageWriter[A]): Array[Byte] = {
      val out   = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(out)
      writer.writeTo(a, pbOut)
      pbOut.flush()
      out.toByteArray
    }
  }
  implicit class PBParserOps(private val bytes: Array[Byte]) extends AnyVal {
    def pbTo[A](implicit reader: PBParser[A]): A = {
      // wraps the bytes into a protobuf single field message
      val out   = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(out)
      pbOut.writeByteArray(1, bytes)
      pbOut.flush()
      reader.parse(1, out.toByteArray)
    }
  }
}
