package pbdirect

import org.scalatest.flatspec._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.Prop._
import com.github.os72.protocjar._
import scala.sys.process._
import java.io._

class ProtocComparisonSpec extends AnyFlatSpec with Checkers {
  import ProtocComparisonSpec._

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  val protoc: File     = Protoc.extractProtoc(ProtocVersion.PROTOC_VERSION, true)
  val workingDir: File = new File(".")
  val protoFile: File  = new File("src/test/resources/proto/ProtocComparisonSpec.proto")
  val protocCommand =
    s"${protoc.getAbsolutePath} --proto_path=${workingDir.getAbsolutePath} --encode=MyMessage $protoFile"

  "pbdirect" should "write the same bytes as protoc does" in check {
    forAllNoShrink { (message: MyMessage) =>
      val textFormattedMessage = toTextFormat(message)
      val in                   = new ByteArrayInputStream(textFormattedMessage.getBytes)
      val out                  = new ByteArrayOutputStream()
      protocCommand.#<(in).#>(out).!
      val protocOutputBytes = out.toByteArray.toList

      val pbdirectOutputBytes = message.toPB.toList

      val label =
        s"""|text formatted message =
            |$textFormattedMessage""".stripMargin

      label |: pbdirectOutputBytes == protocOutputBytes
    }
  }
}

object ProtocComparisonSpec {

  case class MyMessage(
      @pbIndex(2) int: Int,
      @pbIndex(3) packedInts: List[Int],
      @pbIndex(4) @pbUnpacked unpackedInts: List[Int]
  )

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
  def toTextFormat(m: MyMessage): String = {
    s"""|int: ${m.int}
        |packedInts: [${m.packedInts.mkString(", ")}]
        |unpackedInts: [${m.unpackedInts.mkString(", ")}]
        |""".stripMargin
  }

}
