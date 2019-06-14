package pbdirect

import cats.instances.option._
import cats.instances.list._
import org.scalatest.{Matchers, WordSpecLike}

/** Unit tests in which we round-trip objects through PBWriter and PBReader.
  * These tests can cover more complicated objects than those in PBWriterSpec
  * and PBReaderSpec.  However, if there is a bug in both the reader and
  * writer that causes them to create readable but invalid Protobuf data,
  * these tests will not catch it. */
class PBRoundTripSpec extends WordSpecLike with Matchers {
  "A round trip through PBWriter and PBReader" should {
    case class BooleanMessage(value: Boolean)
    case class IntMessage(value: Int)
    case class NestedMessage[A](value: A)
    case class NestedMultiMessage[A,B](valueA: A, valueB: B)
    // A message with 16 fields, which triggers some logic in CodedOutputStream's
    // size calculations:
    case class LargeMessage(
      field0: Int = 0,
      field1: Int = 1,
      field2: Int = 2,
      field3: Int = 3,
      field4: Int = 4,
      field5: Int = 5,
      field6: Int = 6,
      field7: Int = 7,
      field8: Int = 8,
      field9: Int = 9,
      field10: Int = 10,
      field11: Int = 11,
      field12: Int = 12,
      field13: Int = 13,
      field14: Int = 14,
      field15: Int = 15
    )
    
    "preserve a Boolean" in {
      val value = BooleanMessage(false)
      value.toPB.pbTo[BooleanMessage] shouldBe value
    }
    
    "preserve a small Int" in {
      val value = IntMessage(3)
      value.toPB.pbTo[IntMessage] shouldBe value
    }
    
    "preserve a large Int" in {
      val value = IntMessage(Int.MaxValue-3)
      value.toPB.pbTo[IntMessage] shouldBe value
    }
    
    "preserve a negative Int" in {
      val value = IntMessage(Int.MinValue+3)
      value.toPB.pbTo[IntMessage] shouldBe value
    }
    
    "preserve a string" in {
      val value = NestedMessage("foobar")
      value.toPB.pbTo[NestedMessage[String]] shouldBe value
    }
  
    "preserve an empty string" in {
      val value = NestedMessage("")
      value.toPB.pbTo[NestedMessage[String]] shouldBe value
    }
  
    "preserve a string with a variety of Unicode characters" in {
      val value = NestedMessage("\u0020\u25FF\u04FF\u1FFE")
      value.toPB.pbTo[NestedMessage[String]] shouldBe value
    }
    
    "preserve a singly-nested message" in {
      val value = NestedMessage(IntMessage(3))
      value.toPB.pbTo[NestedMessage[IntMessage]] shouldBe value
    }
    
    "preserve a list" in {
      val value = NestedMessage(List(1, 2, 3, 4))
      value.toPB.pbTo[NestedMessage[List[Int]]] shouldBe value
    }
  
    "preserve an empty list" in {
      val value = NestedMessage[List[Int]](Nil)
      value.toPB.pbTo[NestedMessage[List[Int]]] shouldBe value
    }
    
    "preserve a map" in {
      val value = NestedMessage(Map("foo" -> "bar"))
      value.toPB.pbTo[NestedMessage[Map[String,String]]] shouldBe value
    }
    
    "preserve an empty map" in {
      val value = NestedMessage(Map.empty[String,String])
      value.toPB.pbTo[NestedMessage[Map[String,String]]] shouldBe value
    }
    
    "preserve a wrapper(list(wrapper(int)))" in {
      val value = NestedMessage(List(NestedMessage(1), NestedMessage(2)))
      value.toPB.pbTo[NestedMessage[List[NestedMessage[Int]]]] shouldBe value
    }
  
    "preserve a wrapper(list(wrapper(int, string)), string)" in {
      val value = NestedMultiMessage(List(NestedMultiMessage(1, "foo"), NestedMultiMessage(2, "bar")), "garply")
      value.toPB.pbTo[NestedMultiMessage[List[NestedMultiMessage[Int, String]], String]] shouldBe value
    }
    
    "preserve a wrapper(list(wrapper(int, string)), string) with empty list" in {
      val value = NestedMultiMessage[List[Int], String](Nil, "garply")
      value.toPB.pbTo[NestedMultiMessage[List[NestedMultiMessage[Int, String]], String]] shouldBe value
    }
    
    "preserve a wrapper(int, large wrapper)" in {
      val value = NestedMultiMessage[Int, LargeMessage](0, LargeMessage())
      value.toPB.pbTo[NestedMultiMessage[Int, LargeMessage]] shouldBe value
    }
    
    "preserve a wrapper(wrapper(wrapper(optional int)))" in {
      val value = NestedMessage(NestedMessage(NestedMessage(Some(5))))
      value.toPB.pbTo[NestedMessage[NestedMessage[NestedMessage[Option[Int]]]]] shouldBe value
    }
  }
}
