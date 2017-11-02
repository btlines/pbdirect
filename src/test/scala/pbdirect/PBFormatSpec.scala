package pbdirect

import org.scalatest.{Matchers, WordSpec}

class PBFormatSpec extends WordSpec with Matchers {

  "PBFormat" should {
    "derived new instances using imap" in {
      import java.time.Instant
      import cats.syntax.invariant._
      case class Message(instant: Instant)
      implicit val instantFormat: PBFormat[Instant] = PBFormat[Long].imap(Instant.ofEpochMilli)(_.toEpochMilli)
      val instant = Instant.ofEpochMilli(1499411227777L)
      val bytes = Message(instant).toPB
      bytes shouldBe Array[Byte](8, -127, -55, -2, -34, -47, 43)
      bytes.pbTo[Message] shouldBe Message(instant)
    }
  }

}
