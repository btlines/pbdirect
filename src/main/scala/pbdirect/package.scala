import java.io.ByteArrayOutputStream

import com.google.protobuf.{ CodedInputStream, CodedOutputStream }

package object pbdirect {
  implicit class PBWriterOps[A](a: A) {
    def toPB(implicit writer: PBWriter[A]): Array[Byte] = {
      val out = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(out)
      writer.writeTo(1, a, pbOut)
      pbOut.flush()
      val bytes = out.toByteArray
      // remove the tag and return the content
      val input = CodedInputStream.newInstance(bytes)
      input.readTag()
      input.readByteArray()
    }
  }
}
