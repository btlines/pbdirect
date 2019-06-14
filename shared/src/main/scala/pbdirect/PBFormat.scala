package pbdirect

import cats.data.{NonEmptyList => NEL}
import cats.Functor
import cats.{Contravariant, Invariant}
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import pbdirect.LowPriorityPBWriterImplicits.SizeWithoutTag

sealed trait PBFormat[A] extends PBReader[A] with PBWriter[A]

trait PBFormatImplicits {
  implicit object InvariantFormat extends Invariant[PBFormat] {
    override def imap[A, B](format: PBFormat[A])(f: A => B)(g: B => A): PBFormat[B] =
      PBFormat[B](
        Functor[PBReader].map(format)(f),
        Contravariant[PBWriter].contramap(format)(g)
      )
  }
}

object PBFormat extends PBFormatImplicits {

  def apply[A](implicit reader: PBReader[A], writer: PBWriter[A]): PBFormat[A] =
    new PBFormat[A] {
      override def read(input: CodedInputStream): A = reader.read(input)
      override def writeTo(index: NEL[Int], value: A, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
        writer.writeTo(index, value, out, sizes)
      override def writtenBytesSize(index: NEL[Int], value: A, sizes: SizeWithoutTag): Int =
        writer.writtenBytesSize(index, value, sizes)
    }
}
