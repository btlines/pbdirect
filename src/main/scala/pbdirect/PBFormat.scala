package pbdirect

import cats.Functor
import cats.{Contravariant, Invariant}
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import pbdirect.LowPriorityPBWriterImplicits.SizeMap

sealed trait PBFormat[A] extends PBParser[A] with PBWriter[A]

trait PBFormatImplicits {
  implicit object InvariantFormat extends Invariant[PBFormat] {
    override def imap[A, B](format: PBFormat[A])(f: A => B)(g: B => A): PBFormat[B] =
      PBFormat[B](
        Functor[PBParser].map(format)(f),
        Contravariant[PBWriter].contramap(format)(g)
      )
  }
}

object PBFormat extends PBFormatImplicits {
  def apply[A](implicit parser: PBParser[A], writer: PBWriter[A]): PBFormat[A] =
    new PBFormat[A] {
      override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit = parser.readFieldWithoutTag(input, isTopLevel)
      override def build: A = parser.build
      override def componentParsers: List[PBParser[_]] = List(this)
      override def writeTo(index: Int, value: A, out: CodedOutputStream, sizes: SizeMap): Unit =
        writer.writeTo(index, value, out, sizes)
      override def writtenBytesSize(index: Int, value: A, sizes: SizeMap): Int =
        writer.writtenBytesSize(index, value, sizes)
    }
}