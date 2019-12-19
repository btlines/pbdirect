package pbdirect

import cats.Functor
import cats.{Contravariant, Invariant}
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

sealed trait PBFormat[A] extends PBReader[A] with PBFieldWriter[A]

trait PBFormatImplicits {
  implicit object InvariantFormat extends Invariant[PBFormat] {
    override def imap[A, B](format: PBFormat[A])(f: A => B)(g: B => A): PBFormat[B] =
      PBFormat[B](
        Functor[PBReader].map(format)(f),
        Contravariant[PBFieldWriter].contramap(format)(g)
      )
  }
}

object PBFormat extends PBFormatImplicits {
  def apply[A](implicit reader: PBReader[A], writer: PBFieldWriter[A]): PBFormat[A] =
    new PBFormat[A] {
      override def read(input: CodedInputStream): A = reader.read(input)
      override def writeTo(index: Int, value: A, out: CodedOutputStream): Unit =
        writer.writeTo(index, value, out)
    }
}
