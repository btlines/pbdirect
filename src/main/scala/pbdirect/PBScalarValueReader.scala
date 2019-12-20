package pbdirect

import cats.Functor
import com.google.protobuf.CodedInputStream
import shapeless._
import enumeratum.values.{IntEnum, IntEnumEntry}

trait PBScalarValueReader[A] {
  def read(input: CodedInputStream): A
}

trait PBScalarValueReaderImplicits_1 {

  def instance[A](f: CodedInputStream => A): PBScalarValueReader[A] =
    new PBScalarValueReader[A] {
      override def read(input: CodedInputStream): A = f(input)
    }

  implicit def embeddedMessageReader[A](
      implicit reader: PBMessageReader[A]): PBScalarValueReader[A] =
    instance { (input: CodedInputStream) =>
      val bytes = input.readByteArray()
      reader.read(bytes)
    }

}

trait PBScalarValueReaderImplicits extends PBScalarValueReaderImplicits_1 {

  implicit object BooleanReader$ extends PBScalarValueReader[Boolean] {
    override def read(input: CodedInputStream): Boolean = input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ByteReader$ extends PBScalarValueReader[Byte] {
    override def read(input: CodedInputStream): Byte = input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ShortReader$ extends PBScalarValueReader[Short] {
    override def read(input: CodedInputStream): Short = input.readInt32().toShort
  }
  implicit object IntReader$ extends PBScalarValueReader[Int] {
    override def read(input: CodedInputStream): Int = input.readInt32()
  }
  implicit object LongReader$ extends PBScalarValueReader[Long] {
    override def read(input: CodedInputStream): Long = input.readInt64()
  }
  implicit object FloatReader$ extends PBScalarValueReader[Float] {
    override def read(input: CodedInputStream): Float = input.readFloat()
  }
  implicit object DoubleReader$ extends PBScalarValueReader[Double] {
    override def read(input: CodedInputStream): Double = input.readDouble()
  }
  implicit object StringReader$ extends PBScalarValueReader[String] {
    override def read(input: CodedInputStream): String = input.readString()
  }
  implicit object BytesReader$ extends PBScalarValueReader[Array[Byte]] {
    override def read(input: CodedInputStream): Array[Byte] = input.readByteArray()
  }

  implicit object FunctorReader extends Functor[PBScalarValueReader] {
    override def map[A, B](reader: PBScalarValueReader[A])(f: A => B): PBScalarValueReader[B] =
      instance { (input: CodedInputStream) =>
        f(reader.read(input))
      }
  }

  implicit def enumReader[A](
      implicit
      values: Enum.Values[A],
      ordering: Ordering[A],
      reader: PBScalarValueReader[Int]): PBScalarValueReader[A] = instance {
    (input: CodedInputStream) =>
      Enum.fromInt[A](reader.read(input))
  }

  implicit def enumerationReader[E <: Enumeration](
      implicit
      reader: PBScalarValueReader[Int],
      gen: Generic.Aux[E, HNil]): PBScalarValueReader[E#Value] = instance {
    (input: CodedInputStream) =>
      val enum = gen.from(HNil)
      enum(reader.read(input))
  }

  implicit def enumeratumIntEnumEntryReader[E <: IntEnumEntry](
      implicit
      reader: PBScalarValueReader[Int],
      enum: IntEnum[E]): PBScalarValueReader[E] = instance { (input: CodedInputStream) =>
    enum.withValue(reader.read(input))
  }

  implicit def keyValuePairReader[K, V](
      implicit keyReader: PBScalarValueReader[K],
      valueReader: PBScalarValueReader[V]): PBScalarValueReader[(K, V)] = instance {
    (input: CodedInputStream) =>
      val bytes = input.readByteArray()
      val in    = CodedInputStream.newInstance(bytes)
      in.readTag()
      val key = keyReader.read(in)
      in.readTag()
      val value = valueReader.read(in)
      (key, value)
  }

}

object PBScalarValueReader extends PBScalarValueReaderImplicits {
  def apply[A: PBScalarValueReader]: PBScalarValueReader[A] = implicitly
}
