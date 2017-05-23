package pbdirect

import com.google.protobuf.CodedInputStream
import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy }

import scala.util.Try

trait PBExtractor[A] {
  def extract(input: CodedInputStream): A
}
object PBExtractor {
  implicit object BooleanExtractor extends PBExtractor[Boolean] {
    override def extract(input: CodedInputStream): Boolean = input.readBool()
  }
  implicit object IntExtractor extends PBExtractor[Int] {
    override def extract(input: CodedInputStream): Int = input.readInt32()
  }
  implicit object LongExtractor extends PBExtractor[Long] {
    override def extract(input: CodedInputStream): Long = input.readInt64()
  }
  implicit object FloatExtractor extends PBExtractor[Float] {
    override def extract(input: CodedInputStream): Float = input.readFloat()
  }
  implicit object DoubleExtractor extends PBExtractor[Double] {
    override def extract(input: CodedInputStream): Double = input.readDouble()
  }
  implicit object StringExtractor extends PBExtractor[String] {
    override def extract(input: CodedInputStream): String = input.readString()
  }
  implicit object BytesExtractor extends PBExtractor[Array[Byte]] {
    override def extract(input: CodedInputStream): Array[Byte] = input.readByteArray()
  }
}

trait PBReader[A] {
  def read(index: Int, bytes: Array[Byte]): A
}

trait LowerPriorityPBReaderImplicits {
  implicit object CNilReader extends PBReader[List[CNil]] {
    override def read(index: Int, bytes: Array[Byte]): List[CNil] =
      throw new UnsupportedOperationException("Can't read CNil")
  }
  implicit def cconsReader[H, T <: Coproduct](implicit
                                              head: PBReader[List[H]],
                                              tail: Lazy[PBReader[List[T]]]
                                             ): PBReader[List[H :+: T]] = new PBReader[List[H :+: T]] {
    override def read(index: Int, bytes: Array[Byte]): List[H :+: T] =
      Try { head.read(index, bytes).map(Inl.apply) }  getOrElse tail.value.read(index, bytes).map(Inr.apply)
  }
  implicit def coprodReader[A, R <: Coproduct](implicit
                                               gen: Generic.Aux[A, R],
                                               parser: Lazy[PBReader[List[R]]]
                                              ): PBReader[List[A]] = new PBReader[List[A]] {
    override def read(index: Int, bytes: Array[Byte]): List[A] =
      parser.value.read(index, bytes).map(gen.from)
  }
}

trait LowPriorityPBReaderImplicits extends LowerPriorityPBReaderImplicits {
  implicit object HNilReader extends PBReader[HNil] {
    override def read(index: Int, bytes: Array[Byte]): HNil = HNil
  }
  implicit def consReader[H, T <: HList](implicit
    head: PBReader[H],
    tail: Lazy[PBReader[T]]
  ): PBReader[H :: T] = new PBReader[H :: T] {
    override def read(index: Int, bytes: Array[Byte]): H :: T =
      head.read(index, bytes) :: tail.value.read(index + 1, bytes)
  }
  implicit def prodReader[A, R <: HList](implicit
    gen: Generic.Aux[A, R],
    repr: PBReader[R],
    reader: PBReader[List[Array[Byte]]]
  ): PBReader[List[A]] = new PBReader[List[A]] {
    override def read(index: Int, bytes: Array[Byte]): List[A] =
      reader.read(index, bytes).map { bs => gen.from(repr.read(1, bs)) }
  }
  implicit def enumReader[A](implicit
    values: Enum.Values[A],
    ordering: Ordering[A],
    reader: PBReader[List[Int]]
  ): PBReader[List[A]] = new PBReader[List[A]] {
    override def read(index: Int, bytes: Array[Byte]): List[A] =
      reader.read(index, bytes).map(i => Enum.fromInt(i))
  }
  implicit def enumerationReader[E <: Enumeration](implicit
    reader: PBReader[List[Int]],
    gen: Generic.Aux[E, HNil]
  ): PBReader[List[E#Value]] =
  new PBReader[scala.List[E#Value]] {
    override def read(index: Int, bytes: Array[Byte]): List[E#Value] = {
      val enum = gen.from(HNil)
      reader.read(index, bytes).map(enum.apply)
    }
  }
  implicit def requiredReader[A](implicit reader: PBReader[List[A]]): PBReader[A] =
    new PBReader[A] {
      override def read(index: Int, bytes: Array[Byte]): A = reader.read(index, bytes).last
    }
}

trait PBReaderImplicits extends LowPriorityPBReaderImplicits {
  implicit def repeatedReader[A](implicit extractor: PBExtractor[A]): PBReader[List[A]] =
    new PBReader[List[A]] {
      override def read(index: Int, bytes: Array[Byte]): List[A] = {
        val input = CodedInputStream.newInstance(bytes)
        var done = false
        var as: List[A] = Nil
        while (!done) {
          input.readTag() match {
            case 0 => done = true
            case tag if (tag >> 3) == index => as ::= extractor.extract(input)
            case tag => input.skipField(tag)
          }
        }
        as.reverse
      }
    }
  implicit def optionalReader[A](implicit reader: PBReader[List[A]]): PBReader[Option[A]] =
    new PBReader[Option[A]] {
      override def read(index: Int, bytes: Array[Byte]): Option[A] =
        reader.read(index, bytes).lastOption
    }
  implicit def mapReader[K, V](implicit reader: PBReader[List[(K, V)]]): PBReader[Map[K, V]] =
    new PBReader[Map[K, V]] {
      override def read(index: Int, bytes: Array[Byte]): Map[K, V] =
        reader.read(index, bytes).toMap
    }
}

object PBReader extends PBReaderImplicits
