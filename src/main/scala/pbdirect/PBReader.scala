package pbdirect

import java.io.ByteArrayOutputStream

import cats.Functor
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import shapeless.HList.unsafeGet
import shapeless.ops.hlist
import shapeless.ops.hlist._
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy, Poly1}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

trait PBReader[A] {
  def read(input: CodedInputStream, isTopLevel: Boolean): A
}

trait PBParser[A] {
  def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit
  def build: A
  def readSingleFieldAndBuild(input: CodedInputStream, isTopLevel: Boolean): A = {
    readFieldWithoutTag(input, isTopLevel)
    build
  }
  def componentParsers: List[PBParser[_]] = List(this)
}

trait LowerPriorityPBReaderImplicits {
  def instance[A](readF: (CodedInputStream, Boolean) => A): PBReader[A] = new PBReader[A] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): A = readF(input, isTopLevel)
  }
  
  implicit def coprodReader[A, R <: Coproduct](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBReader[A] = instance { (input: CodedInputStream, isTopLevel: Boolean) =>
    gen.from(repr.value.readSingleFieldAndBuild(input, isTopLevel=isTopLevel))
  }
}

trait PBReaderImplicits extends LowerPriorityPBReaderImplicits {
  
  implicit def prodReader[A, R <: HList](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBReader[A] = instance { (input: CodedInputStream, isTopLevel: Boolean) =>
    gen.from(repr.value.readSingleFieldAndBuild(input, isTopLevel=isTopLevel))
  }
  
  implicit def enumReader[A](implicit
    values: Enum.Values[A],
    ordering: Ordering[A],
    parser: PBReader[Int]
  ): PBReader[A] = instance { (input: CodedInputStream, isTopLevel: Boolean) =>
    Enum.fromInt[A](parser.read(input, isTopLevel))
  }
  implicit def enumerationReader[E <: Enumeration](implicit
    parser: PBReader[Int],
    gen: Generic.Aux[E, HNil]
  ): PBReader[E#Value] = instance { (input: CodedInputStream, isTopLevel: Boolean) =>
    val enum = gen.from(HNil)
    enum(parser.read(input, isTopLevel))
  }
}

object PBReader extends PBReaderImplicits {
  implicit object BooleanReader$ extends PBReader[Boolean] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): Boolean = input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ByteReader$ extends PBReader[Byte] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): Byte = input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ShortReader$ extends PBReader[Short] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): Short = input.readInt32().toShort
  }
  implicit object IntReader$ extends PBReader[Int] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): Int = input.readInt32()
  }
  implicit object LongReader$ extends PBReader[Long] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): Long = input.readInt64()
  }
  implicit object FloatReader$ extends PBReader[Float] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): Float = input.readFloat()
  }
  implicit object DoubleReader$ extends PBReader[Double] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): Double = input.readDouble()
  }
  implicit object StringReader$ extends PBReader[String] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): String = input.readString()
  }
  implicit object BytesReader$ extends PBReader[Array[Byte]] {
    override def read(input: CodedInputStream, isTopLevel: Boolean): Array[Byte] = input.readByteArray()
  }
  
  def apply[A : PBReader]: PBReader[A] = implicitly
  
  implicit object FunctorReader extends Functor[PBReader] {
    override def map[A, B](reader: PBReader[A])(f: A => B): PBReader[B] = instance {
      (input: CodedInputStream, isTopLevel: Boolean) => f(reader.read(input, isTopLevel))
    }
  }
}

trait LowPriorityPBParserImplicits {
  def exactlyOnceParser[A](readF: (CodedInputStream, Boolean) => A): PBParser[A] = new PBParser[A] {
    private var value: Option[A] = None
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit = {
      value = Some(readF(input, isTopLevel))
    }
    override def build: A = value.get
  }
  
  def possiblyRepeatedParser[A, B](readFieldWithoutTagF: (CodedInputStream, Boolean) => A)(builder: mutable.IndexedSeq[A] => B): PBParser[B] = new PBParser[B] {
    private var values: mutable.ArrayBuffer[A] = new ArrayBuffer[A]()
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit = {
      values += readFieldWithoutTagF(input, isTopLevel)
    }
    override def build: B = builder(values)
  }
  def possiblyRepeatedParserWithReader[A, B](reader: PBReader[A])(builder: mutable.IndexedSeq[A] => B): PBParser[B] =
    possiblyRepeatedParser(reader.read)(builder)
  
  implicit def cconsParser[H, T <: Coproduct](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :+: T] = exactlyOnceParser { (input: CodedInputStream, isTopLevel: Boolean) =>
    //FIXME: Currently top-level coproducts are unsupported, because we have no
    // way (with this API) to get the number of bytes to read.
    require(!isTopLevel)
    // Unfortunately (for efficiency, at least), we must read the input as a
    // byte array here, because we have to interpret it potentially multiple
    // times.
    val bytes = input.readByteArray()
    Try {
      Inl(head.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), isTopLevel=false))
    } getOrElse {
      Inr(tail.value.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), isTopLevel=false))
    }
  }
  implicit val cnilParser: PBParser[CNil] = new PBParser[CNil] {
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit =
      throw new UnsupportedOperationException("Can't read HNil")
    override def build: CNil = throw new UnsupportedOperationException("Can't build CNil")
    override def componentParsers: List[PBParser[_]] = Nil
  }
  
  implicit def consParser[H, T <: HList](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :: T] = new PBParser[H :: T] {
    override val componentParsers: List[PBParser[_]] = head :: tail.value.componentParsers
    
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit = {
      // Top-level products have no size tag and the size limit is implicit in the
      // byte buffer's size.
      val sizeLimit = if (!isTopLevel)
        input.readRawVarint32()
      else
        Int.MaxValue // This is the value used in CodedInputStream to signal no limit.
  
      val oldLimit = if (sizeLimit < Int.MaxValue) input.pushLimit(sizeLimit) else Int.MaxValue
      
      val parsers = componentParsers.toIndexedSeq
      val numParsers = parsers.length
      
      var done = false
      var numInputsRead = 0
      while (!done) {
        val tag = input.readTag()
        val oneBasedIndex = tag >> 3
        if (tag == 0) {
          done = true
        } else if (oneBasedIndex > numParsers) {
          input.skipField(tag)
        } else {
          parsers(oneBasedIndex-1).readFieldWithoutTag(input, isTopLevel=false)
        }
        numInputsRead += 1
      }
      input.popLimit(oldLimit)
    }
    
    override def build: H :: T = head.build :: tail.value.build
  }
  implicit val hnilParser: PBParser[HNil] = new PBParser[HNil] {
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit =
      throw new UnsupportedOperationException("Can't read HNil")
    override def build: HNil = HNil
    override def componentParsers: List[PBParser[_]] = Nil
  }
}

trait PBParserImplicits extends LowPriorityPBParserImplicits {
  implicit def repeatedParser[A](implicit reader: PBReader[A]): PBParser[List[A]] =
    possiblyRepeatedParserWithReader(reader) { values: mutable.IndexedSeq[A] => values.toList }
  implicit def requiredParser[A](implicit reader: PBReader[A]): PBParser[A] =
    possiblyRepeatedParserWithReader(reader) { values: mutable.IndexedSeq[A] => values.last }
  implicit def optionalParser[A](implicit reader: PBReader[A]): PBParser[Option[A]] =
    possiblyRepeatedParserWithReader(reader) { values: mutable.IndexedSeq[A] => values.lastOption }
  implicit def mapParser[K, V](implicit reader: PBReader[(K, V)]): PBParser[Map[K, V]] =
    possiblyRepeatedParserWithReader(reader) { values: mutable.IndexedSeq[(K, V)] => values.toMap }
  implicit def collectionMapParser[K, V](implicit reader: PBReader[(K, V)]): PBParser[collection.Map[K, V]] =
    possiblyRepeatedParserWithReader(reader) { values: mutable.IndexedSeq[(K, V)] => values.toMap }
  implicit def seqParser[A](implicit reader: PBReader[A]): PBParser[Seq[A]] =
    possiblyRepeatedParserWithReader(reader) { values: mutable.IndexedSeq[A] => values.toSeq}
  implicit def indexedSeqParser[A](implicit reader: PBReader[A]): PBParser[IndexedSeq[A]] =
    possiblyRepeatedParserWithReader(reader) { values: mutable.IndexedSeq[A] => values.toIndexedSeq }
}

object PBParser extends PBParserImplicits