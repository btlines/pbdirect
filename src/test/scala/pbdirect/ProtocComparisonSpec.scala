package pbdirect

import org.scalatest.flatspec._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.Prop._
import com.github.os72.protocjar._
import scala.sys.process._
import java.io._
import shapeless._
import org.scalacheck.Prop

class ProtocComparisonSpec extends AnyFlatSpec with Checkers {
  import ProtocComparisonSpec._

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  val protoc: File     = Protoc.extractProtoc(ProtocVersion.PROTOC_VERSION, true)
  val workingDir: File = new File(".")
  val protoFile: File  = new File("src/test/resources/proto/ProtocComparisonSpec.proto")
  val protocCommand =
    s"${protoc.getAbsolutePath} --proto_path=${workingDir.getAbsolutePath} --encode=MessageThree $protoFile"

  "pbdirect" should "write the same bytes as protoc does" in check {
    forAllNoShrink { (message: MessageThree) =>
      val pbdirectOutputBytes = message.toPB.toList

      val textFormattedMessage = TextFormatEncoding.messageThree(message)
      val in                   = new ByteArrayInputStream(textFormattedMessage.getBytes)
      val out                  = new ByteArrayOutputStream()
      val protocExitCode       = protocCommand.#<(in).#>(out).!
      val protocOutputBytes    = awaitProtocOutput(out)

      val label =
        s"""|_bytes = ${message._bytes.toList}
            |
            |text formatted message =
            |$textFormattedMessage
            |
            |binary output of pbdirect =
            |$pbdirectOutputBytes
            |
            |binary output of protoc =
            |$protocOutputBytes""".stripMargin

      label |: Prop.all(
        s"protoc exit code = $protocExitCode" |: protocExitCode == 0,
        "bytes should match" |: pbdirectOutputBytes == protocOutputBytes
      )
    }
  }

  /*
   * Workaround for test flakiness on Travis.
   * Occasionally the output buffer is not written until after the process
   * has exited. (I think it happens on a separate thread.)
   * So we wait to give the data a chance to show up.
   */
  def awaitProtocOutput(out: ByteArrayOutputStream): List[Byte] = {
    var retriesLeft = 10
    while (out.size() == 0 && retriesLeft > 0) {
      println("Protoc output is still empty. Waiting...")
      Thread.sleep(500L)
      retriesLeft -= 1
    }

    out.toByteArray.toList
  }

}

object ProtocComparisonSpec {

  case class MessageOne(
      dbl: Double,
      boolean: Boolean
  )

  case class MessageTwo(
      long: Long,
      messageOne: MessageOne,
      messageOneOption: Option[MessageOne],
      @pbIndex(4, 5, 6) coproduct: Option[MessageOne :+: MessageTwo :+: Int :+: CNil]
  )

  case class MessageThree(
      @pbIndex(2) int: Int,
      @pbIndex(3) packedInts: List[Int],
      @pbIndex(4) @pbUnpacked unpackedInts: List[Int],
      @pbIndex(5) intOption: Option[Int],
      @pbIndex(6) stringStringMap: Map[String, String],
      @pbIndex(7) _string: String,
      @pbIndex(8) _bytes: Array[Byte],
      @pbIndex(9) messageOne: MessageOne,
      @pbIndex(10) messageOneOption: Option[MessageOne],
      @pbIndex(11) messageTwo: MessageTwo,
      @pbIndex(12) intMessageTwoMap: Map[Int, MessageTwo]
  )

  object TextFormatEncoding {

    /*
     * Note:
     * we could define some kind of Encoder[A] type class
     * to encode a value of type A as protobuf text format.
     * But then we'd be basically reinventing the whole
     * library but for text instead of binary, and then using
     * that to test the behaviour of the library. Feels a bit
     * weird.
     *
     * So instead we keep things simple: write out the encoder
     * by hand and only support the exact types used in the test.
     */

    def indent(string: String): String =
      string.split('\n').map(line => s"  $line").mkString("\n")

    def messageOne(m: MessageOne): String =
      s"""|dbl: ${m.dbl}
          |boolean: ${m.boolean}""".stripMargin

    def embeddedMessageOne(m: MessageOne): String =
      s"""|{
          |${indent(messageOne(m))}
          |}""".stripMargin

    def option[A](fieldName: String, opt: Option[A])(f: A => String): String =
      opt.fold("")(a => s"$fieldName: ${f(a)}")

    def coproduct(cop: MessageOne :+: MessageTwo :+: Int :+: CNil): String = {
      cop match {
        case Inl(messageOne)      => s"a: ${embeddedMessageOne(messageOne)}"
        case Inr(Inl(messageTwo)) => s"b: ${embeddedMessageTwo(messageTwo)}"
        case Inr(Inr(Inl(int)))   => s"c: $int"
        case _                    => ""
      }
    }

    def messageTwo(m: MessageTwo): String =
      s"""|long: ${m.long}
          |messageOne: ${embeddedMessageOne(m.messageOne)}
          |${option("messageOneOption", m.messageOneOption)(embeddedMessageOne)}
          |${m.coproduct.fold("")(coproduct)}
          |""".stripMargin

    def embeddedMessageTwo(m: MessageTwo): String =
      s"""|{
          |${indent(messageTwo(m))}
          |}""".stripMargin

    def bytes(xs: Array[Byte]): String =
      xs.map(b => s"\\${(b.toInt & 0xFF).toOctalString}").mkString

    def string(x: String): String = {
      val escaped = x
        .replaceAllLiterally("""\""", """\\""") // escape backslashes
        .replaceAllLiterally("\n", "\\n") // escape newlines and other control characters
        .replaceAllLiterally("\r", "\\r")
        .replaceAllLiterally("\b", "\\b")
        .replaceAllLiterally("\f", "\\f")
        .replaceAllLiterally("\t", "\\t")
        .replaceAllLiterally("\u0000", "\\0")
        .replaceAllLiterally(""""""", """\"""") // esape double quotes
      s""""$escaped"""" // wrap in double quotes
    }

    def stringStringMap(map: Map[String, String]): String = {
      map
        .map {
          case (key, value) =>
            s"""|stringStringMap: {
                |  key: ${string(key)}
                |  value: ${string(value)}
                |}""".stripMargin
        }
        .mkString("\n")
    }

    def intMessageTwoMap(map: Map[Int, MessageTwo]): String = {
      map
        .map {
          case (key, value) =>
            s"""|intMessageTwoMap: {
                |  key: ${key}
                |  value: {
                |${indent(indent(messageTwo(value)))}
                |  }
                |}""".stripMargin
        }
        .mkString("\n")
    }

    def messageThree(m: MessageThree): String = {
      s"""|int: ${m.int}
          |packedInts: [${m.packedInts.mkString(", ")}]
          |unpackedInts: [${m.unpackedInts.mkString(", ")}]
          |${option("intOption", m.intOption)(_.toString)}
          |${stringStringMap(m.stringStringMap)}
          |_string: ${string(m._string)}
          |_bytes: "${bytes(m._bytes)}"
          |messageOne: ${embeddedMessageOne(m.messageOne)}
          |${option("messageOneOption", m.messageOneOption)(embeddedMessageOne)}
          |messageTwo: ${embeddedMessageTwo(m.messageTwo)}
          |${intMessageTwoMap(m.intMessageTwoMap)}
          |""".stripMargin
    }

  }

}
