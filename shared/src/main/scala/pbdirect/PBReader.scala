package pbdirect

import java.io.ByteArrayOutputStream

import cats.Functor
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

import scala.util.Try

trait PBReader[A] {
  def read(input: CodedInputStream): A
}
trait LowerPriorityPBReaderImplicits {
  def instance[A](f: CodedInputStream => A): PBReader[A] =
    new PBReader[A] {
      override def read(input: CodedInputStream): A = f(input)
    }
  implicit def coprodReader[A, R <: Coproduct](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBReader[A] = instance { (input: CodedInputStream) =>
    val bytes = input.readByteArray()

    // wraps the bytes into a protobuf single field message
    val out = new ByteArrayOutputStream()
    val pbOut = CodedOutputStream.newInstance(out)
    pbOut.writeByteArray(1, bytes)
    pbOut.flush()

    gen.from(repr.value.parse(1, out.toByteArray))
  }
}
trait PBReaderImplicits extends LowerPriorityPBReaderImplicits {

  implicit def prodReader[A, R <: HList](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBReader[A] = instance { (input: CodedInputStream) =>
    val bytes = input.readByteArray()
    gen.from(repr.value.parse(1, bytes))
  }

  implicit def enumReader[A](implicit
    values: Enum.Values[A],
    ordering: Ordering[A],
    reader: PBReader[Int]
  ): PBReader[A] = instance { (input: CodedInputStream) =>
    Enum.fromInt[A](reader.read(input))
  }
  implicit def enumerationReader[E <: Enumeration](implicit
    reader: PBReader[Int],
    gen: Generic.Aux[E, HNil]
  ): PBReader[E#Value] = instance { (input: CodedInputStream) =>
    val enum = gen.from(HNil)
    enum(reader.read(input))
  }
}
object PBReader extends PBReaderImplicits {
  implicit object BooleanReader$ extends PBReader[Boolean] {
    override def read(input: CodedInputStream): Boolean = input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ByteReader$ extends PBReader[Byte] {
    override def read(input: CodedInputStream): Byte = input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ShortReader$ extends PBReader[Short] {
    override def read(input: CodedInputStream): Short = input.readInt32().toShort
  }
  implicit object IntReader$ extends PBReader[Int] {
    override def read(input: CodedInputStream): Int = input.readInt32()
  }
  implicit object LongReader$ extends PBReader[Long] {
    override def read(input: CodedInputStream): Long = input.readInt64()
  }
  implicit object FloatReader$ extends PBReader[Float] {
    override def read(input: CodedInputStream): Float = input.readFloat()
  }
  implicit object DoubleReader$ extends PBReader[Double] {
    override def read(input: CodedInputStream): Double = input.readDouble()
  }
  implicit object StringReader$ extends PBReader[String] {
    override def read(input: CodedInputStream): String = input.readString()
  }
  implicit object BytesReader$ extends PBReader[Array[Byte]] {
    override def read(input: CodedInputStream): Array[Byte] = input.readByteArray()
  }

  def apply[A : PBReader]: PBReader[A] = implicitly

  implicit object FunctorReader extends Functor[PBReader] {
    override def map[A, B](reader: PBReader[A])(f: A => B): PBReader[B] = instance {
      (input: CodedInputStream) => f(reader.read(input))
    }
  }
}

trait PBParser[A] {
  def parse(index: Int, bytes: Array[Byte]): A
}

trait LowPriorityPBParserImplicits {
  def instance[A](f: (Int, Array[Byte]) => A): PBParser[A] = new PBParser[A] {
    override def parse(index: Int, bytes: Array[Byte]): A = f(index, bytes)
  }
  implicit val hnilParser: PBParser[HNil] = instance {
    (index: Int, bytes: Array[Byte]) => HNil
  }
  implicit def consParser[H, T <: HList](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :: T] = instance { (index: Int, bytes: Array[Byte]) =>
    head.parse(index, bytes) :: tail.value.parse(index + 1, bytes)
  }

  implicit val cnilParser: PBParser[CNil] = instance {
    (index: Int, bytes: Array[Byte]) =>
      throw new UnsupportedOperationException("Can't read CNil")
  }
  implicit def cconsParser[H, T <: Coproduct](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :+: T] = instance { (index: Int, bytes: Array[Byte]) =>
    Try {
      Inl(head.parse(index, bytes))
    } getOrElse {
      Inr(tail.value.parse(index, bytes))
    }
  }
}

trait PBParserImplicits extends LowPriorityPBParserImplicits {
  implicit def repeatedParser[A](implicit reader: PBReader[A]): PBParser[List[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input = CodedInputStream.newInstance(bytes)
      var done = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0 => done = true
          case tag if (tag >> 3) == index => as ::= reader.read(input)
          case tag => input.skipField(tag)
        }
      }
      as.reverse
    }
  implicit def requiredParser[A](implicit reader: PBReader[A]): PBParser[A] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input = CodedInputStream.newInstance(bytes)
      var done = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0 => done = true
          case tag if (tag >> 3) == index => as ::= reader.read(input)
          case tag => input.skipField(tag)
        }
      }
      as.head
    }
  implicit def optionalParser[A](implicit parser: PBParser[List[A]]): PBParser[Option[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).lastOption
    }
  implicit def mapParser[K, V](implicit parser: PBParser[List[(K, V)]]): PBParser[Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).toMap
    }
  implicit def collectionMapParser[K, V](implicit parser: PBParser[List[(K, V)]]): PBParser[collection.Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).toMap
    }
  implicit def seqParser[A](implicit parser: PBParser[List[A]]): PBParser[Seq[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes)
    }
}

object PBParser extends PBParserImplicits
