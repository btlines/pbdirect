package pbdirect

import cats.Functor
import cats.syntax.functor._
import com.google.protobuf.CodedInputStream
import shapeless._
import enumeratum.values.{IntEnum, IntEnumEntry}

trait PBScalarValueReader[A] {

  /**
   * The default value to use as a fallback if the field is missing from the message.
   */
  def defaultValue: A

  /**
   * Whether it's possible for a repeated field of this type to be encoded using the packed encoding.
   * This is only possible for "primitive" protobuf types:
   * int{32,64}, {u,s}int{32,64}, bool, enum, fixed32, sfixed32, float, fixed64, sfixed64, double
   */
  def canBePacked: Boolean

  /**
   * Read a value of type A from the current point in the input stream.
   */
  def read(input: CodedInputStream): A
}

trait PBScalarValueReaderImplicits_1 {

  implicit def embeddedMessageReader[A](
      implicit reader: PBMessageReader[A]): PBScalarValueReader[A] =
    new PBScalarValueReader[A] {
      // To construct a default instance of the message
      // we decode a byte array of length zero,
      // i.e. with all of the message's fields missing
      val defaultValue: A      = reader.read(Array[Byte]())
      def canBePacked: Boolean = false
      def read(input: CodedInputStream): A = {
        val bytes = input.readByteArray()
        reader.read(bytes)
      }
    }

}

trait PBScalarValueReaderImplicits extends PBScalarValueReaderImplicits_1 {

  implicit object BooleanReader$ extends PBScalarValueReader[Boolean] {
    override def defaultValue: Boolean                  = false
    override def canBePacked: Boolean                   = true
    override def read(input: CodedInputStream): Boolean = input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ByteReader$ extends PBScalarValueReader[Byte] {
    override def defaultValue: Byte                  = 0.toByte
    override def canBePacked: Boolean                = true
    override def read(input: CodedInputStream): Byte = input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ShortReader$ extends PBScalarValueReader[Short] {
    override def defaultValue: Short                  = 0.toShort
    override def canBePacked: Boolean                 = true
    override def read(input: CodedInputStream): Short = input.readInt32().toShort
  }
  implicit object IntReader$ extends PBScalarValueReader[Int] {
    override def defaultValue: Int                  = 0
    override def canBePacked: Boolean               = true
    override def read(input: CodedInputStream): Int = input.readInt32()
  }
  implicit object LongReader$ extends PBScalarValueReader[Long] {
    override def defaultValue: Long                  = 0L
    override def canBePacked: Boolean                = true
    override def read(input: CodedInputStream): Long = input.readInt64()
  }
  implicit object FloatReader$ extends PBScalarValueReader[Float] {
    override def defaultValue: Float                  = 0.0F
    override def canBePacked: Boolean                 = true
    override def read(input: CodedInputStream): Float = input.readFloat()
  }
  implicit object DoubleReader$ extends PBScalarValueReader[Double] {
    override def defaultValue: Double                  = 0.0
    override def canBePacked: Boolean                  = true
    override def read(input: CodedInputStream): Double = input.readDouble()
  }
  implicit object StringReader$ extends PBScalarValueReader[String] {
    override def defaultValue: String                  = ""
    override def canBePacked: Boolean                  = false
    override def read(input: CodedInputStream): String = input.readString()
  }
  implicit object BytesReader$ extends PBScalarValueReader[Array[Byte]] {
    override def defaultValue: Array[Byte]                  = Array()
    override def canBePacked: Boolean                       = false
    override def read(input: CodedInputStream): Array[Byte] = input.readByteArray()
  }

  implicit object FunctorReader extends Functor[PBScalarValueReader] {
    override def map[A, B](reader: PBScalarValueReader[A])(f: A => B): PBScalarValueReader[B] =
      new PBScalarValueReader[B] {
        val defaultValue: B                  = f(reader.defaultValue)
        def canBePacked: Boolean             = reader.canBePacked
        def read(input: CodedInputStream): B = f(reader.read(input))
      }
  }

  implicit def enumReader[A](
      implicit
      values: Enum.Values[A],
      ordering: Ordering[A],
      reader: PBScalarValueReader[Int]): PBScalarValueReader[A] =
    new PBScalarValueReader[A] {
      val defaultValue: A                  = Enum.fromInt[A](0)
      def canBePacked: Boolean             = true
      def read(input: CodedInputStream): A = Enum.fromInt[A](reader.read(input))
    }

  implicit def enumerationReader[E <: Enumeration](
      implicit
      reader: PBScalarValueReader[Int],
      gen: Generic.Aux[E, HNil]): PBScalarValueReader[E#Value] = {
    val enum = gen.from(HNil)
    new PBScalarValueReader[E#Value] {
      val defaultValue: E#Value                  = enum(0)
      def canBePacked: Boolean                   = true
      def read(input: CodedInputStream): E#Value = enum(reader.read(input))
    }
  }

  implicit def enumeratumIntEnumEntryReader[E <: IntEnumEntry](
      implicit
      reader: PBScalarValueReader[Int],
      enum: IntEnum[E]): PBScalarValueReader[E] =
    new PBScalarValueReader[E] {
      val defaultValue: E                  = enum.withValue(0)
      def canBePacked: Boolean             = true
      def read(input: CodedInputStream): E = enum.withValue(reader.read(input))
    }

  implicit def keyValuePairReader[K, V](
      implicit keyReader: PBScalarValueReader[K],
      valueReader: PBScalarValueReader[V]): PBScalarValueReader[(K, V)] = {
    val defaultKey   = keyReader.defaultValue
    val defaultValue = valueReader.defaultValue
    val defaultPair  = (defaultKey, defaultValue)
    new PBScalarValueReader[(K, V)] {
      val defaultValue: (K, V) = defaultPair
      def canBePacked: Boolean = false
      def read(input: CodedInputStream): (K, V) = {
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

  implicit def leftReader[A, B](
      implicit reader: PBScalarValueReader[A]): PBScalarValueReader[Left[A, B]] =
    reader.map(Left(_))

  implicit def rightReader[A, B](
      implicit reader: PBScalarValueReader[B]): PBScalarValueReader[Right[A, B]] =
    reader.map(Right(_))

}

object PBScalarValueReader extends PBScalarValueReaderImplicits {
  def apply[A: PBScalarValueReader]: PBScalarValueReader[A] = implicitly
}
