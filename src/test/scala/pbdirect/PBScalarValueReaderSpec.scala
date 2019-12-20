package pbdirect

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import com.google.protobuf.CodedInputStream

class PBScalarValueReaderSpec extends AnyWordSpecLike with Matchers {

  "PBScalarValueReader" should {
    "derive new instance using map" in {
      import java.time.Instant
      import cats.syntax.functor._
      implicit val instantReader: PBScalarValueReader[Instant] =
        PBScalarValueReader[Long].map(Instant.ofEpochMilli)
      val instant = Instant.ofEpochMilli(1499411227777L)
      val bytes   = Array[Byte](-127, -55, -2, -34, -47, 43)
      val in      = CodedInputStream.newInstance(bytes)
      PBScalarValueReader[Instant].read(in) shouldBe (instant)
    }
  }

}
