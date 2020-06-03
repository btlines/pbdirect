/*
 * Copyright (c) 2017-2020 47 Degrees Open Source <https://www.47deg.com>
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

// Copyright (c) 2017-2020 47 Degrees Open Source <https://www.47deg.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package pbdirect

import org.scalatest.flatspec._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import enumeratum.values.IntEnumEntry
import enumeratum.values.IntEnum
import shapeless._
import shapeless.tag.@@
import shapeless.ops.hlist._

sealed abstract class Status(val value: Int) extends IntEnumEntry
object Status extends IntEnum[Status] {
  case object Running extends Status(0)
  case object Stopped extends Status(5)
  val values = findValues
}

class RoundTripSpec extends AnyFlatSpec with Checkers {
  import RoundTripSpec._

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  implicit class PBEquivalenceSyntax[A](val a: A)(implicit equivalence: PBEquivalence[A]) {
    def equiv(a2: A): Boolean = equivalence.equiv(a, a2)
  }

  implicit def signedIntArb(implicit int: Arbitrary[Int]): Arbitrary[Int @@ Signed] =
    Arbitrary(int.arbitrary.map(tag[Signed](_)))

  implicit val fixedWidthIntArb: Arbitrary[Int @@ Fixed] =
    Arbitrary(Gen.posNum[Int].map(tag[Fixed](_)))

  "round trip to protobuf and back" should "result in a message equivalent to the original" in check {
    forAllNoShrink { (message: MessageThree) =>
      val serialized   = message.toPB
      val roundtripped = serialized.pbTo[MessageThree]

      val label = s"""|
        |message before roundtrip = $message
        |
        |message after  roundtrip = $roundtripped
        |""".stripMargin

      label |: (message equiv roundtripped)
    }
  }

}

object RoundTripSpec {

  case class EmptyMessage()

  case class MessageOne(
      @pbIndex(1) optionalEmpty: Option[EmptyMessage],
      @pbIndex(2) boolean: Boolean,
      @pbIndex(3) repeatedFloat: List[Float],
      @pbIndex(4) @pbUnpacked repeatedUnpackedFloat: List[Float]
  )

  case class MessageTwo(
      @pbIndex(5) int: Int,
      @pbIndex(10) string: String,
      @pbIndex(15) emptyMessage: EmptyMessage,
      @pbIndex(20) nestedMessage: MessageOne,
      @pbIndex(21, 22, 23) coproduct: Option[Int :+: String :+: MessageOne :+: CNil],
      @pbIndex(24, 25) either: Option[Either[Int, String]],
      @pbIndex(26) signedInt: Int @@ Signed,
      @pbIndex(27) fixedWidthInt: Int @@ Fixed
  )

  case class MessageThree(
      @pbIndex(2) int: Int,
      @pbIndex(4) optionalInt: Option[Int],
      @pbIndex(6) boolean: Boolean,
      @pbIndex(8) optionalBoolean: Option[Boolean],
      @pbIndex(10) double: Double,
      @pbIndex(12) float: Float,
      @pbIndex(14) long: Long,
      @pbIndex(16) string: String,
      @pbIndex(18) repeatedString: List[String],
      @pbIndex(20) enum: Status,
      @pbIndex(22) repeatedEnum: List[Status],
      @pbIndex(24) optionalEnum: Option[Status],
      @pbIndex(26) byte: Byte,
      @pbIndex(28) short: Short,
      @pbIndex(30) bytes: Array[Byte],
      @pbIndex(32) intStringMap: Map[Int, String],
      @pbIndex(34) stringBoolListMap: Map[String, List[Boolean]],
      @pbIndex(36) nestedMessage: MessageTwo,
      @pbIndex(38) optionalNestedMessage: Option[MessageTwo],
      @pbIndex(40) repeatedNestedMessage: List[MessageTwo],
      @pbIndex(42) intMessageMap: Map[Int, MessageTwo]
  )

}

/**
 * We define a commutative equivalence relation for protobuf-encoded data
 * with the following laws:
 *
 * - ∀ x. x == x => x equiv x
 *   (all data is equivalent to itself)
 *
 * - ∀ scalar types T. Some(d: T) equiv None iff d is the protobuf default value for type T
 *   (because the data is not written on the wire, it's reasonable to decode it as None)
 *
 * - ∀ scalar types T. None equiv Some(d: T) iff d is the protobuf default value for type T
 *   (because it's also reasonable to decode a missing Option as Some(defaultValue)
 *
 * - ∀ messages m1(x1, x2, ..., xN), m2(y1, y2, ..., yN).
 *     (∀ 1 ≦ n ≦ N. xn equiv yn) => m1 equiv m2
 *   (two messages are equivalent if all their fields are equivalent)
 *
 */
trait PBEquivalence[A] {
  def equiv(a1: A, a2: A): Boolean
  def show: String
  override def toString(): String = show
}

trait PBEquivalenceImplicits_2 {

  def instance[A](description: String)(f: (A, A) => Boolean): PBEquivalence[A] =
    new PBEquivalence[A] {
      override def equiv(a1: A, a2: A): Boolean = f(a1, a2)
      override def show: String                 = description
    }

  implicit def refl[A]: PBEquivalence[A] = instance("refl")((a1, a2) => a1 == a2)

}

trait PBEquivalenceImplicits_1 extends PBEquivalenceImplicits_2 {

  // special treatment for arrays because == doesn't work (it uses reference equality)
  implicit def array[A](implicit listEquiv: PBEquivalence[List[A]]): PBEquivalence[Array[A]] =
    instance("array")((a1, a2) => listEquiv.equiv(a1.toList, a2.toList))

  def option[A](description: String, defaultValue: A)(implicit
      equiv: PBEquivalence[A]
  ): PBEquivalence[Option[A]] =
    instance(description) {
      case (Some(x), None) if equiv.equiv(x, defaultValue) => true
      case (Some(_), None)                                 => false
      case (None, Some(x)) if equiv.equiv(x, defaultValue) => true
      case (None, Some(_))                                 => false
      case (None, None)                                    => true
      case (Some(x), Some(y))                              => equiv.equiv(x, y)
    }

  implicit val byteOption: PBEquivalence[Option[Byte]]     = option("byteOption", 0.toByte)
  implicit val shortOption: PBEquivalence[Option[Short]]   = option("shortOption", 0.toShort)
  implicit val intOption: PBEquivalence[Option[Int]]       = option("intOption", 0)
  implicit val longOption: PBEquivalence[Option[Long]]     = option("longOption", 0L)
  implicit val floatOption: PBEquivalence[Option[Float]]   = option("floatOption", 0.0f)
  implicit val doubleOption: PBEquivalence[Option[Double]] = option("doubleOption", 0.0)
  implicit val boolOption: PBEquivalence[Option[Boolean]]  = option("boolOption", false)
  implicit val stringOption: PBEquivalence[Option[String]] = option("stringOption", "")
  implicit val bytesOption: PBEquivalence[Option[Array[Byte]]] =
    option("bytesOption", Array.empty[Byte])
  implicit val enumOption: PBEquivalence[Option[Status]] =
    option("enumOption", Status.withValue(0))

  implicit def either[A, B](implicit
      aEquiv: PBEquivalence[A],
      bEquiv: PBEquivalence[B]
  ): PBEquivalence[Either[A, B]] =
    instance("either") {
      case (Left(a1), Left(a2))   => aEquiv.equiv(a1, a2)
      case (Right(b1), Right(b2)) => bEquiv.equiv(b1, b2)
      case _                      => false
    }

  object fieldEquivalence extends Poly2 {
    implicit def defaultCase[A](implicit equiv: PBEquivalence[A]): Case.Aux[A, A, Boolean] =
      at[A, A](equiv.equiv(_, _))
  }

  object conj extends Poly2 {
    implicit val defaultCase: Case.Aux[Boolean, Boolean, Boolean] =
      at[Boolean, Boolean](_ && _)
  }

  implicit val hnil: PBEquivalence[HNil] = instance("hnil")((_, _) => true)

  // two messages of type A are equivalent if all their fields are equivalent
  implicit def message[A, R <: HList, FEs <: HList](implicit
      gen: Generic.Aux[A, R],
      fieldEquivs: ZipWith.Aux[R, R, fieldEquivalence.type, FEs],
      fold: LeftFolder.Aux[FEs, Boolean, conj.type, Boolean]
  ): PBEquivalence[A] =
    instance("message") {
      case (a1, a2) =>
        val hlist1   = gen.to(a1)
        val hlist2   = gen.to(a2)
        val booleans = hlist1.zipWith(hlist2)(fieldEquivalence) // fieldEquivs.apply(hlist1, hlist2)
        val result   = booleans.foldLeft(true)(conj)
        if (!result)
          println(booleans)
        result
    }

}

object PBEquivalence extends PBEquivalenceImplicits_1 {
  def apply[A: PBEquivalence]: PBEquivalence[A] = implicitly
}
