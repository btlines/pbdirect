import java.io.ByteArrayOutputStream

import com.google.protobuf.CodedOutputStream

package object pbdirect {

  implicit class PBMessageWriterOps[A](private val a: A) extends AnyVal {
    def toPB(implicit writer: PBMessageWriter[A]): Array[Byte] = {
      val out   = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(out)
      writer.writeTo(a, pbOut)
      pbOut.flush()
      out.toByteArray
    }
  }

  implicit class PBMessageReaderOps(private val bytes: Array[Byte]) extends AnyVal {
    def pbTo[A](implicit reader: PBMessageReader[A]): A =
      reader.read(bytes)
  }

}
