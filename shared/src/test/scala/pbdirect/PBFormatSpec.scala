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
        _1: Int,
        _2: Int,
        _3: Int,
        _4: Int,
        _5: Int,
        _6: Int,
        _7: Int,
        _8: Int,
        _9: Int,
        _10: Int,
        _11: Int,
        _12: Int,
        _13: Int,
        _14: Int,
        _15: Int,
        _16: Int,
        _17: Int,
        _18: Int,
        _19: Int,
        _20: Int,
        _21: Int,
        _22: Int,
        _23: Int,
        _24: Int,
        _25: Int,
        _26: Int,
        _27: Int,
        _28: Int,
        _29: Int,
        _30: Int,
        _31: Int
      )
      val record = Record(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
        27, 28, 29, 30, 31)
      val bytes = Array[Byte](8, 1, 16, 2, 24, 3, 32, 4, 40, 5, 48, 6, 56, 7, 64, 8, 72, 9, 80, 10, 88, 11, 96, 12, 104,
        13, 112, 14, 120, 15, -128, 1, 16, -120, 1, 17, -112, 1, 18, -104, 1, 19, -96, 1, 20, -88, 1, 21, -80, 1, 22,
        -72, 1, 23, -64, 1, 24, -56, 1, 25, -48, 1, 26, -40, 1, 27, -32, 1, 28, -24, 1, 29, -16, 1, 30, -8, 1, 31)
      record.toPB shouldBe bytes
      bytes.pbTo[Record] shouldBe record
    }
    "format a sealed trait with many implementations" in {
      sealed trait Record
      sealed trait Record2
      sealed trait Record4
      sealed trait Record8
      case class R1(@Index(1) value: Int) extends Record with Record2 with Record4 with Record8
      case class R2(@Index(2) value: Int) extends Record with Record2 with Record4 with Record8
      case class R3(@Index(3) value: Int) extends Record with Record2 with Record4 with Record8
      case class R4(@Index(4) value: Int) extends Record with Record4 with Record8
      case class R5(@Index(5) value: Int) extends Record with Record4 with Record8
      case class R6(@Index(6) value: Int) extends Record with Record8
      case class R7(@Index(7) value: Int) extends Record with Record8
      case class R8(@Index(8) value: Int) extends Record with Record8
      case class R9(@Index(9) value: Int) extends Record with Record8
      case class R10(@Index(10) value: Int) extends Record
      case class R11(@Index(11) value: Int) extends Record
      case class R12(@Index(12) value: Int) extends Record
      case class R13(@Index(13) value: Int) extends Record
      case class R14(@Index(14) value: Int) extends Record
      case class R15(@Index(15) value: Int) extends Record
      case class R16(@Index(16) value: Int) extends Record
      case class R17(@Index(17) value: Int) extends Record
      val r1 = R1(1)
      val r1Bytes = Array[Byte](8, 1)
      (r1: Record).toPB shouldBe r1Bytes
      (r1: Record2).toPB shouldBe r1Bytes
      (r1: Record4).toPB shouldBe r1Bytes
      (r1: Record8).toPB shouldBe r1Bytes
      r1Bytes.pbTo[Record] shouldBe r1
      r1Bytes.pbTo[Record2] shouldBe r1
      r1Bytes.pbTo[Record4] shouldBe r1
      r1Bytes.pbTo[Record8] shouldBe r1
      val r2 = R2(2)
      val r2Bytes = Array[Byte](16, 2)
      (r2: Record).toPB shouldBe r2Bytes
      (r2: Record2).toPB shouldBe r2Bytes
      (r2: Record4).toPB shouldBe r2Bytes
      (r2: Record8).toPB shouldBe r2Bytes
      r2Bytes.pbTo[Record] shouldBe r2
      r2Bytes.pbTo[Record2] shouldBe r2
      r2Bytes.pbTo[Record4] shouldBe r2
      r2Bytes.pbTo[Record8] shouldBe r2
      val r3 = R3(3)
      val r3Bytes = Array[Byte](24, 3)
      (r3: Record).toPB shouldBe r3Bytes
      (r3: Record2).toPB shouldBe r3Bytes
      (r3: Record4).toPB shouldBe r3Bytes
      (r3: Record8).toPB shouldBe r3Bytes
      r3Bytes.pbTo[Record] shouldBe r3
      r3Bytes.pbTo[Record2] shouldBe r3
      r3Bytes.pbTo[Record4] shouldBe r3
      r3Bytes.pbTo[Record8] shouldBe r3
      val r4 = R4(4)
      val r4Bytes = Array[Byte](32, 4)
      (r4: Record).toPB shouldBe r4Bytes
      (r4: Record4).toPB shouldBe r4Bytes
      (r4: Record8).toPB shouldBe r4Bytes
      r4Bytes.pbTo[Record] shouldBe r4
      r4Bytes.pbTo[Record4] shouldBe r4
      r4Bytes.pbTo[Record8] shouldBe r4
      val r5 = R5(5)
      val r5Bytes = Array[Byte](40, 5)
      (r5: Record).toPB shouldBe r5Bytes
      (r5: Record4).toPB shouldBe r5Bytes
      (r5: Record8).toPB shouldBe r5Bytes
      r5Bytes.pbTo[Record] shouldBe r5
      r5Bytes.pbTo[Record4] shouldBe r5
      r5Bytes.pbTo[Record8] shouldBe r5
      val r6 = R6(6)
      val r6Bytes = Array[Byte](48, 6)
      (r6: Record).toPB shouldBe r6Bytes
      (r6: Record8).toPB shouldBe r6Bytes
      r6Bytes.pbTo[Record] shouldBe r6
      r6Bytes.pbTo[Record8] shouldBe r6
      val r7 = R7(7)
      val r7Bytes = Array[Byte](56, 7)
      (r7: Record).toPB shouldBe r7Bytes
      (r7: Record8).toPB shouldBe r7Bytes
      r7Bytes.pbTo[Record] shouldBe r7
      r7Bytes.pbTo[Record8] shouldBe r7
      val r8 = R8(8)
      val r8Bytes = Array[Byte](64, 8)
      (r8: Record).toPB shouldBe r8Bytes
      (r8: Record8).toPB shouldBe r8Bytes
      r8Bytes.pbTo[Record] shouldBe r8
      r8Bytes.pbTo[Record8] shouldBe r8
      val r9 = R9(9)
      val r9Bytes = Array[Byte](72, 9)
      (r9: Record).toPB shouldBe r9Bytes
      (r9: Record8).toPB shouldBe r9Bytes
      r9Bytes.pbTo[Record] shouldBe r9
      r9Bytes.pbTo[Record8] shouldBe r9
      val r10 = R10(10)
      val r10Bytes = Array[Byte](80, 10)
      (r10: Record).toPB shouldBe r10Bytes
      r10Bytes.pbTo[Record] shouldBe r10
      val r11 = R11(11)
      val r11Bytes = Array[Byte](88, 11)
      (r11: Record).toPB shouldBe r11Bytes
      r11Bytes.pbTo[Record] shouldBe r11
      val r12 = R12(12)
      val r12Bytes = Array[Byte](96, 12)
      (r12: Record).toPB shouldBe r12Bytes
      r12Bytes.pbTo[Record] shouldBe r12
      val r13 = R13(13)
      val r13Bytes = Array[Byte](104, 13)
      (r13: Record).toPB shouldBe r13Bytes
      r13Bytes.pbTo[Record] shouldBe r13
      val r14 = R14(14)
      val r14Bytes = Array[Byte](112, 14)
      (r14: Record).toPB shouldBe r14Bytes
      r14Bytes.pbTo[Record] shouldBe r14
      val r15 = R15(15)
      val r15Bytes = Array[Byte](120, 15)
      (r15: Record).toPB shouldBe r15Bytes
      r15Bytes.pbTo[Record] shouldBe r15
      val r16 = R16(16)
      val r16Bytes = Array[Byte](-128, 1, 16)
      (r16: Record).toPB shouldBe r16Bytes
      r16Bytes.pbTo[Record] shouldBe r16
      val r17 = R17(17)
      val r17Bytes = Array[Byte](-120, 1, 17)
      (r17: Record).toPB shouldBe r17Bytes
      r17Bytes.pbTo[Record] shouldBe r17
    }
  }

}
