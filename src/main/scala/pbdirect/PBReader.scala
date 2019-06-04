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
  def read(input: CodedInputStream, size: Option[Int]): A
}

trait PBParser[A] {
  def readFieldWithoutTag(input: CodedInputStream, size: Option[Int]): Unit
  def build: A
  def readSingleFieldAndBuild(input: CodedInputStream, size: Option[Int]): A = {
    readFieldWithoutTag(input, size)
    build
  }
  def componentParsers: List[PBParser[_]] = List(this)
}

trait LowerPriorityPBReaderImplicits {
  def instance[A](readF: (CodedInputStream, Option[Int]) => A): PBReader[A] = new PBReader[A] {
    override def read(input: CodedInputStream, size: Option[Int]): A = readF(input, size)
  }
  
  implicit def coprodReader[A, R <: Coproduct](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBReader[A] = instance { (input: CodedInputStream, size: Option[Int]) =>
    gen.from(repr.value.readSingleFieldAndBuild(input, size=size))
  }
}

trait PBReaderImplicits extends LowerPriorityPBReaderImplicits {
  
  implicit def prodReader[A, R <: HList](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBReader[A] = instance { (input: CodedInputStream, size: Option[Int]) =>
    gen.from(repr.value.readSingleFieldAndBuild(input, size=size))
  }
  
  implicit def enumReader[A](implicit
    values: Enum.Values[A],
    ordering: Ordering[A],
    parser: PBReader[Int]
  ): PBReader[A] = instance { (input: CodedInputStream, size: Option[Int]) =>
    Enum.fromInt[A](parser.read(input, size))
  }
  implicit def enumerationReader[E <: Enumeration](implicit
    parser: PBReader[Int],
    gen: Generic.Aux[E, HNil]
  ): PBReader[E#Value] = instance { (input: CodedInputStream, size: Option[Int]) =>
    val enum = gen.from(HNil)
    enum(parser.read(input, size))
  }
}

object PBReader extends PBReaderImplicits {
  implicit object BooleanReader$ extends PBReader[Boolean] {
    override def read(input: CodedInputStream, size: Option[Int]): Boolean = input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ByteReader$ extends PBReader[Byte] {
    override def read(input: CodedInputStream, size: Option[Int]): Byte = input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit object ShortReader$ extends PBReader[Short] {
    override def read(input: CodedInputStream, size: Option[Int]): Short = input.readInt32().toShort
  }
  implicit object IntReader$ extends PBReader[Int] {
    override def read(input: CodedInputStream, size: Option[Int]): Int = input.readInt32()
  }
  implicit object LongReader$ extends PBReader[Long] {
    override def read(input: CodedInputStream, size: Option[Int]): Long = input.readInt64()
  }
  implicit object FloatReader$ extends PBReader[Float] {
    override def read(input: CodedInputStream, size: Option[Int]): Float = input.readFloat()
  }
  implicit object DoubleReader$ extends PBReader[Double] {
    override def read(input: CodedInputStream, size: Option[Int]): Double = input.readDouble()
  }
  implicit object StringReader$ extends PBReader[String] {
    override def read(input: CodedInputStream, size: Option[Int]): String = input.readString()
  }
  implicit object BytesReader$ extends PBReader[Array[Byte]] {
    override def read(input: CodedInputStream, size: Option[Int]): Array[Byte] = input.readByteArray()
  }
  
  def apply[A : PBReader]: PBReader[A] = implicitly
  
  implicit object FunctorReader extends Functor[PBReader] {
    override def map[A, B](reader: PBReader[A])(f: A => B): PBReader[B] = instance {
      (input: CodedInputStream, size: Option[Int]) => f(reader.read(input, size))
    }
  }
}

trait LowPriorityPBParserImplicits {
  def exactlyOnceParser[A](readF: (CodedInputStream, Option[Int]) => A): PBParser[A] = new PBParser[A] {
    private var value: Option[A] = None
    override def readFieldWithoutTag(input: CodedInputStream, size: Option[Int]): Unit = {
      value = Some(readF(input, size))
    }
    override def build: A = {
      val v = value.get
      value = None
      v
    }
  }
  
  def possiblyRepeatedParser[A, B](sizeHint: Int)(readFieldWithoutTagF: (CodedInputStream, Option[Int]) => A)(builder: mutable.IndexedSeq[A] => B): PBParser[B] = new PBParser[B] {
    private var values: mutable.ArrayBuffer[A] = null
    override def readFieldWithoutTag(input: CodedInputStream, size: Option[Int]): Unit = {
      if (values == null) values = new ArrayBuffer[A](sizeHint)
      values += readFieldWithoutTagF(input, size)
    }
    override def build: B = {
      if (values == null) values = new ArrayBuffer[A](initialSize=0)
      val v = builder(values)
      values = null
      v
    }
  }
  def possiblyRepeatedParserWithReader[A, B](sizeHint: Int)(reader: PBReader[A])(builder: mutable.IndexedSeq[A] => B): PBParser[B] =
    possiblyRepeatedParser(sizeHint)(reader.read)(builder)
  
  implicit def cconsParser[H, T <: Coproduct](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :+: T] = exactlyOnceParser { (input: CodedInputStream, size: Option[Int]) =>
    // Unfortunately (for efficiency, at least), we must read the input as a
    // byte array here, because we have to interpret it potentially multiple
    // times.
    val bytes = size match {
      // Our parent provided our size, which is a signal that the size field
      // is not in the stream.
      case Some(sz) => input.readRawBytes(sz)
      // Our parent was unable to provide a size for us, which is a signal
      // that the size field must still be available in the stream.
      case None => input.readByteArray()
    }
    Try {
      Inl(head.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))
    } getOrElse {
      Inr(tail.value.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))
    }
  }
  implicit val cnilParser: PBParser[CNil] = new PBParser[CNil] {
    override def readFieldWithoutTag(input: CodedInputStream, size: Option[Int]): Unit =
      throw new UnsupportedOperationException("Can't read HNil")
    override def build: CNil = throw new UnsupportedOperationException("Can't build CNil")
    override def componentParsers: List[PBParser[_]] = Nil
  }
  
  implicit def consParser[H, T <: HList](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :: T] = new PBParser[H :: T] {
    private lazy val componentParsersVec = {
      val numParsers = componentParsers.size
      if (numParsers > 2) componentParsers.toIndexedSeq
      else componentParsers
    }
    override val componentParsers: List[PBParser[_]] = head :: tail.value.componentParsers
    
    override def readFieldWithoutTag(input: CodedInputStream, size: Option[Int]): Unit = {
      val sizeLimit = size match {
        // Our parent provided our size, which is a signal that the size field
        // is not in the stream.
        case Some(sz) => sz
        // Our parent was unable to provide a size for us, which is a signal
        // that the size field must still be available in the stream.
        case None => input.readRawVarint32()
      }
      
      val oldLimit = input.pushLimit(sizeLimit)
      
      val numParsers = componentParsersVec.size
      
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
          componentParsersVec(oneBasedIndex-1).readFieldWithoutTag(input, size=None)
        }
        numInputsRead += 1
      }
      
      input.popLimit(oldLimit)
    }
    
    override def build: H :: T = head.build :: tail.value.build
  }
  implicit val hnilParser: PBParser[HNil] = new PBParser[HNil] {
    override def readFieldWithoutTag(input: CodedInputStream, size: Option[Int]): Unit = ()
    override def build: HNil = HNil
    override def componentParsers: List[PBParser[_]] = Nil
  }
}

trait PBParserImplicits extends LowPriorityPBParserImplicits {
  private val SINGLETON_SIZE_HINT = 1
  private val COLLECTION_SIZE_HINT = 16
  
  implicit def repeatedParser[A](implicit reader: PBReader[A]): PBParser[List[A]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values.toList }
  implicit def requiredParser[A](implicit reader: PBReader[A]): PBParser[A] =
    possiblyRepeatedParserWithReader(sizeHint=SINGLETON_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values.last }
  implicit def optionalParser[A](implicit reader: PBReader[A]): PBParser[Option[A]] =
    possiblyRepeatedParserWithReader(sizeHint=SINGLETON_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values.lastOption }
  implicit def mapParser[K, V](implicit reader: PBReader[(K, V)]): PBParser[Map[K, V]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[(K, V)] => values.toMap }
  implicit def collectionMapParser[K, V](implicit reader: PBReader[(K, V)]): PBParser[collection.Map[K, V]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[(K, V)] =>
      val result = mutable.HashMap.empty[K, V]
      result.sizeHint(values) //NOTE: Seems to do nothing, unfortunately.
      result ++= values
      result
    }
  implicit def seqParser[A](implicit reader: PBReader[A]): PBParser[Seq[A]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values }
  implicit def indexedSeqParser[A](implicit reader: PBReader[A]): PBParser[IndexedSeq[A]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values }
}

object PBParser extends PBParserImplicits