package pbdirect

import org.scalatest.{ Matchers, WordSpecLike }

class EnumSpec extends WordSpecLike with Matchers {

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
      Enum.values[WeekDay] shouldBe Monday :: Tuesday :: Wednesday :: Thursday :: Friday :: Saturday :: Sunday :: Nil
    }
    "get correct position for a value" in {
      Enum.toInt[WeekDay](Monday)    shouldBe 1
      Enum.toInt[WeekDay](Tuesday)   shouldBe 2
      Enum.toInt[WeekDay](Wednesday) shouldBe 3
      Enum.toInt[WeekDay](Thursday)  shouldBe 4
      Enum.toInt[WeekDay](Friday)    shouldBe 5
      Enum.toInt[WeekDay](Saturday)  shouldBe 6
      Enum.toInt[WeekDay](Sunday)    shouldBe 7
    }
    "get correct value for a position" in {
      Enum.fromInt[WeekDay](1) shouldBe Monday
      Enum.fromInt[WeekDay](2) shouldBe Tuesday
      Enum.fromInt[WeekDay](3) shouldBe Wednesday
      Enum.fromInt[WeekDay](4) shouldBe Thursday
      Enum.fromInt[WeekDay](5) shouldBe Friday
      Enum.fromInt[WeekDay](6) shouldBe Saturday
      Enum.fromInt[WeekDay](7) shouldBe Sunday
    }
  }
}
