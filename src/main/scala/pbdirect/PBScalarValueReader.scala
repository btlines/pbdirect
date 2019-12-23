package pbdirect

import cats.Functor
import com.google.protobuf.CodedInputStream
import shapeless._
import enumeratum.values.{IntEnum, IntEnumEntry}

trait PBScalarValueReader[A] {
  def defaultValue: A
  def read(input: CodedInputStream): A
}

trait PBScalarValueReaderImplicits_1 {

  def instance[A](default: A)(f: CodedInputStream => A): PBScalarValueReader[A] =
    new PBScalarValueReader[A] {
      override def defaultValue: A                  = default
      override def read(input: CodedInputStream): A = f(input)
    }

  implicit def embeddedMessageReader[A](
      implicit reader: PBMessageReader[A]): PBScalarValueReader[A] = {

    // To construct a default instance of the message
    // we decode a byte array of length zero,
    // i.e. with all of the message's fields missing
    val default: A = reader.read(Array[Byte]())

    instance(default) { (input: CodedInputStream) =>
      val bytes = input.readByteArray()
      reader.read(bytes)
    }
  }

}

trait PBScalarValueReaderImplicits extends PBScalarValueReaderImplicits_1 {

  implicit object BooleanReader$ extends PBScalarValueReader[Boolean] {
    override def defaultValue: Boolean                  = false
    override def read(input: CodedInputStream): Boolean = input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ByteReader$ extends PBScalarValueReader[Byte] {
    override def defaultValue: Byte                  = 0.toByte
    override def read(input: CodedInputStream): Byte = input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ShortReader$ extends PBScalarValueReader[Short] {
    override def defaultValue: Short                  = 0.toShort
    override def read(input: CodedInputStream): Short = input.readInt32().toShort
  }
  implicit object IntReader$ extends PBScalarValueReader[Int] {
    override def defaultValue: Int                  = 0
    override def read(input: CodedInputStream): Int = input.readInt32()
  }
  implicit object LongReader$ extends PBScalarValueReader[Long] {
    override def defaultValue: Long                  = 0L
    override def read(input: CodedInputStream): Long = input.readInt64()
  }
  implicit object FloatReader$ extends PBScalarValueReader[Float] {
    override def defaultValue: Float                  = 0.0F
    override def read(input: CodedInputStream): Float = input.readFloat()
  }
  implicit object DoubleReader$ extends PBScalarValueReader[Double] {
    override def defaultValue: Double                  = 0.0
    override def read(input: CodedInputStream): Double = input.readDouble()
  }
  implicit object StringReader$ extends PBScalarValueReader[String] {
    override def defaultValue: String                  = ""
    override def read(input: CodedInputStream): String = input.readString()
  }
  implicit object BytesReader$ extends PBScalarValueReader[Array[Byte]] {
    override def defaultValue: Array[Byte]                  = Array()
    override def read(input: CodedInputStream): Array[Byte] = input.readByteArray()
  }

  implicit object FunctorReader extends Functor[PBScalarValueReader] {
    override def map[A, B](reader: PBScalarValueReader[A])(f: A => B): PBScalarValueReader[B] =
      instance(default = f(reader.defaultValue)) { (input: CodedInputStream) =>
        f(reader.read(input))
      }
  }

  implicit def enumReader[A](
      implicit
      values: Enum.Values[A],
      ordering: Ordering[A],
      reader: PBScalarValueReader[Int]): PBScalarValueReader[A] =
    instance(default = Enum.fromInt[A](0)) { (input: CodedInputStream) =>
      Enum.fromInt[A](reader.read(input))
    }

  implicit def enumerationReader[E <: Enumeration](
      implicit
      reader: PBScalarValueReader[Int],
      gen: Generic.Aux[E, HNil]): PBScalarValueReader[E#Value] = {
    val enum    = gen.from(HNil)
    val default = enum(0)
    instance[E#Value](default) { (input: CodedInputStream) =>
      enum(reader.read(input))
    }
  }

  implicit def enumeratumIntEnumEntryReader[E <: IntEnumEntry](
      implicit
      reader: PBScalarValueReader[Int],
      enum: IntEnum[E]): PBScalarValueReader[E] =
    instance(default = enum.withValue(0)) { (input: CodedInputStream) =>
      enum.withValue(reader.read(input))
    }

  implicit def keyValuePairReader[K, V](
      implicit keyReader: PBScalarValueReader[K],
      valueReader: PBScalarValueReader[V]): PBScalarValueReader[(K, V)] = {
    val defaultKey   = keyReader.defaultValue
    val defaultValue = valueReader.defaultValue
    val default      = (defaultKey, defaultValue)
    instance(default) { (input: CodedInputStream) =>
      val bytes = input.readByteArray()
      val in    = CodedInputStream.newInstance(bytes)
      in.readTag()
      val key = keyReader.read(in)
      in.readTag()
      val value = valueReader.read(in)
      (key, value)
    }
  }

}

object PBScalarValueReader extends PBScalarValueReaderImplicits {
  def apply[A: PBScalarValueReader]: PBScalarValueReader[A] = implicitly
}
