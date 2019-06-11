package pbdirect

import org.scalatest.{Matchers, WordSpec}

class PBFormatSpec extends WordSpec with Matchers {
  import java.time.Instant
  import cats.syntax.invariant._

  implicit val instantFormat: PBFormat[Instant] = PBFormat[Long].imap(Instant.ofEpochMilli)(_.toEpochMilli)

  "PBFormat" should {
    "derived new instances using imap" in {
      case class Message(instant: Instant)
      val instant = Instant.ofEpochMilli(1499411227777L)
      val bytes = Message(instant).toPB
      bytes shouldBe Array[Byte](8, -127, -55, -2, -34, -47, 43)
      bytes.pbTo[Message] shouldBe Message(instant)
    }
    "derived optional instances using imap" in {
      import cats.instances.option._
      case class Message(instant: Option[Instant])
      val instant = Instant.ofEpochMilli(1499411227777L)
      val bytes = Message(Some(instant)).toPB
      bytes shouldBe Array[Byte](8, -127, -55, -2, -34, -47, 43)
      bytes.pbTo[Message] shouldBe Message(Some(instant))
    }
  }

}
