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
import org.scalatest.wordspec.AnyWordSpecLike

class EnumSpec extends AnyWordSpecLike with Matchers {

  sealed trait WeekDay  extends Pos
  case object Monday    extends WeekDay with Pos._1
  case object Tuesday   extends WeekDay with Pos._2
  case object Wednesday extends WeekDay with Pos._3
  case object Thursday  extends WeekDay with Pos._4
  case object Friday    extends WeekDay with Pos._5
  case object Saturday  extends WeekDay with Pos._6
  case object Sunday    extends WeekDay with Pos._7

  "Enum" should {
    "list values in declared order" in {
      Enum
        .values[WeekDay] shouldBe Monday :: Tuesday :: Wednesday :: Thursday :: Friday :: Saturday :: Sunday :: Nil
    }
    "get correct position for a value" in {
      Enum.toInt[WeekDay](Monday) shouldBe 0
      Enum.toInt[WeekDay](Tuesday) shouldBe 1
      Enum.toInt[WeekDay](Wednesday) shouldBe 2
      Enum.toInt[WeekDay](Thursday) shouldBe 3
      Enum.toInt[WeekDay](Friday) shouldBe 4
      Enum.toInt[WeekDay](Saturday) shouldBe 5
      Enum.toInt[WeekDay](Sunday) shouldBe 6
    }
    "get correct value for a position" in {
      Enum.fromInt[WeekDay](0) shouldBe Monday
      Enum.fromInt[WeekDay](1) shouldBe Tuesday
      Enum.fromInt[WeekDay](2) shouldBe Wednesday
      Enum.fromInt[WeekDay](3) shouldBe Thursday
      Enum.fromInt[WeekDay](4) shouldBe Friday
      Enum.fromInt[WeekDay](5) shouldBe Saturday
      Enum.fromInt[WeekDay](6) shouldBe Sunday
    }
  }
}
