package pbdirect

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import java.io.ByteArrayOutputStream
import com.google.protobuf.CodedOutputStream

class PBScalarValueWriterSpec extends AnyWordSpecLike with Matchers {

  def write[A](value: A)(implicit writer: PBScalarValueWriter[A]): Array[Byte] = {
    val fieldWriter = implicitly[PBFieldWriter[A]]
    val buffer      = new ByteArrayOutputStream()
    val out         = CodedOutputStream.newInstance(buffer)
    fieldWriter.writeTo(1, value, out, skipDefaultValue = true)
    out.flush()
    buffer.toByteArray()
  }

  "PBScalarValueWriter" should {
    "derive new instance using contramap" in {
      import java.time.Instant
      import cats.syntax.contravariant._
      case class Message(@pbIndex(1) instant: Instant)
      implicit val instantWriter: PBScalarValueWriter[Instant] =
        PBScalarValueWriter[Long].contramap(_.toEpochMilli)
      val instant = Instant.ofEpochMilli(1499411227777L)
      write(instant) shouldBe Array[Byte](8, -127, -55, -2, -34, -47, 43)
    }
  }

}
