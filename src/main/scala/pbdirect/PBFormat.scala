package pbdirect

import cats.Functor
import cats.{Contravariant, Invariant}
import com.google.protobuf.CodedOutputStream

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
      override def read(index: Int, bytes: Array[Byte]): A =
        reader.read(index, bytes)
      override def writeTo(index: Int, value: A, out: CodedOutputStream): Unit =
        writer.writeTo(index, value, out)
    }
}