package pbdirect

import cats.Functor
import cats.{Contravariant, Invariant}
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

sealed trait PBFormat[A] extends PBScalarValueReader[A] with PBScalarValueWriter[A]

trait PBFormatImplicits {
  implicit object InvariantFormat extends Invariant[PBFormat] {
    override def imap[A, B](format: PBFormat[A])(f: A => B)(g: B => A): PBFormat[B] =
      PBFormat[B](
        Functor[PBScalarValueReader].map(format)(f),
        Contravariant[PBScalarValueWriter].contramap(format)(g)
      )
  }
}

object PBFormat extends PBFormatImplicits {
  def apply[A](
      implicit reader: PBScalarValueReader[A],
      writer: PBScalarValueWriter[A]
  ): PBFormat[A] =
    new PBFormat[A] {
      override def read(input: CodedInputStream): A = reader.read(input)
      override def wireType: Int                    = writer.wireType
      override def isDefault(value: A): Boolean     = writer.isDefault(value)
      override def defaultValue: A                  = reader.defaultValue
      override def canBePacked: Boolean             = reader.canBePacked
      override def writeWithoutTag(value: A, out: CodedOutputStream): Unit =
        writer.writeWithoutTag(value, out)
    }
}
