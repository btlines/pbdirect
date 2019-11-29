/*
 * Copyright (c) 2019 Beyond the lines
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package pbdirect

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PBFormatSpec extends AnyWordSpec with Matchers {
  import java.time.Instant
  import cats.syntax.invariant._

  implicit val instantFormat: PBFormat[Instant] =
    PBFormat[Long].imap(Instant.ofEpochMilli)(_.toEpochMilli)

  "PBFormat" should {
    "derived new instances using imap" in {
      case class Message(instant: Instant)
      val instant = Instant.ofEpochMilli(1499411227777L)
      val bytes   = Message(instant).toPB
      bytes shouldBe Array[Byte](8, -127, -55, -2, -34, -47, 43)
      bytes.pbTo[Message] shouldBe Message(instant)
    }
    "derived optional instances using imap" in {
      import cats.instances.option._
      case class Message(instant: Option[Instant])
      val instant = Instant.ofEpochMilli(1499411227777L)
      val bytes   = Message(Some(instant)).toPB
      bytes shouldBe Array[Byte](8, -127, -55, -2, -34, -47, 43)
      bytes.pbTo[Message] shouldBe Message(Some(instant))
    }
  }

}
