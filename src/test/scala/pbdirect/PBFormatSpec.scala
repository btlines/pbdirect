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
    "format a case class with many fields" in {
      case class Record(
        one: String, two: Int, three: Int, four: Int, five: Int,
        six: Long, seven: String, eight: String, nine: String, ten: String,
        eleven: String, twelve: String, thirteen: Int, fourteen: String, fifteen: String,
        sixteen: String, seventeen: String, srcPort: String, dstPort: String, appProtocol: String,
        tUlBytes: Long, tDlBytes: Long, startTimeEpoch: Long, endTimeEpoch: Long, eighteen: Long,
        nineteen: Long, twenty: String, twentyone: String, twentytwo: Int, twentythree: String,
        twentyfour: String, twentyfive: String, twentysix: String, twentyseven: String, twentyeight: Int,
        twentynine: Int, thirty: String, thirtyone: Int, thirtytwo : String, thirtythree : String,
        thirtyfour : String, thirtyfive : String, thirtysix : String, thirtyseven : Int, thirtyeight : Int,
        ipServerIpAddress : String, ipProtocol : Int, ipSrcAddress : String,
        snChargeVolumeIpBytesDownlink : Int, snChargeVolumeIpBytesUplink : Int, thirtynine : Int, forty : String,
        fortyone : String, fortytwo : String, fortythree : String, fortyfour : String, fortyfive : Int,
        fortysix : String, fortyseven : String, fortyeight : Int, fortynine : Int, fifty : Int,
        fiftyone : Int, fiftytwo : String, fiftythree : String, fiftyfour : String, fiftyfive : String,
        fiftysix : String, fiftyseven : String, fiftyeight : String, fiftynine : String, sixty : Double,
        sixtyone : Double, sixtytwo : String, sixtythree : String, sixtyfour : String, sixtyfive : String,
        sixtysix : String, sixtyseven : String
      )
      val rec = new Record(
        "", 0, 0, 0, 0,
        0L, "", "", "", "",
        "", "", 0, "", "",
        "", "", "", "", "",
        0L, 0L, 0L, 0L, 0L,
        0L, "", "", 0, "",
        "", "", "", "", 0,
        0, "", 0, "0", "",
        "", "", "", 0, 0,
        "", 0, "",
        0, 0, 0, "",
        "", "", "", "", 0,
        "", "", 0, 0, 0,
        0, "", "", "", "",
        "", "", "", "", 0.0,
        0.0, "", "", "", "",
        "", ""
      )
      val bytes = rec.toPB
      bytes.pbTo[Record] shouldBe rec
    }
  }

}
