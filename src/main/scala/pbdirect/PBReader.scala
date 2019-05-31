package pbdirect

import java.io.ByteArrayOutputStream

import cats.Functor
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import shapeless.HList.unsafeGet
import shapeless.ops.hlist
import shapeless.ops.hlist._
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy, Poly1}

import scala.collection.mutable
import scala.util.Try

trait PBReader[A] {
  def read(input: CodedInputStream): A
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

trait LowPriorityPBParserImplicits {
  def exactlyOnce[A](readFieldWithoutTagF: (CodedInputStream, Boolean) => A): PBParser[A] = new PBParser[A] {
    private var value: Option[A] = None
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit = {
      value = Some(readFieldWithoutTagF(input, isTopLevel))
    }
    override def build: A = value.get
  }
  def possiblyRepeatedWithParser[A, B](parser: PBParser[A])(builder: List[A] => B): PBParser[B] =
    possiblyRepeated(parser.readSingleFieldAndBuild)(builder)
  def possiblyRepeated[A, B](readFieldWithoutTagF: (CodedInputStream, Boolean) => A)(builder: List[A] => B): PBParser[B] = new PBParser[B] {
    private var values: List[A] = Nil
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit = {
      values ::= readFieldWithoutTagF(input, isTopLevel)
    }
    override def build: B = builder(values)
  }
  
  implicit def cconsParser[H, T <: Coproduct](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :+: T] = exactlyOnce { (input: CodedInputStream, isTopLevel: Boolean) =>
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
  implicit def coprodParser[A, R <: Coproduct](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBParser[A] = exactlyOnce { (input: CodedInputStream, isTopLevel: Boolean) =>
    gen.from(repr.value.readSingleFieldAndBuild(input, isTopLevel=isTopLevel))
  }
  
  implicit def prodParser[A, R <: HList](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBParser[A] = new PBParser[A] {
    private var value: Option[A] = None
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit = {
      value = Some(gen.from(repr.value.readSingleFieldAndBuild(input, isTopLevel=isTopLevel)))
    }
    override def build: A = value.get
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
}

trait PBParserImplicits extends LowPriorityPBParserImplicits {
  implicit def enumReader[A](implicit
    values: Enum.Values[A],
    ordering: Ordering[A],
    parser: PBParser[Int]
  ): PBParser[A] = exactlyOnce { (input: CodedInputStream, isTopLevel: Boolean) =>
    Enum.fromInt[A](parser.readSingleFieldAndBuild(input, isTopLevel))
  }
  implicit def enumerationReader[E <: Enumeration](implicit
    parser: PBParser[Int],
    gen: Generic.Aux[E, HNil]
  ): PBParser[E#Value] = exactlyOnce { (input: CodedInputStream, isTopLevel: Boolean) =>
    val enum = gen.from(HNil)
    enum(parser.readSingleFieldAndBuild(input, isTopLevel))
  }
  implicit val hnilParser: PBParser[HNil] = new PBParser[HNil] {
    override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit = ()
    override def build: HNil = HNil
    override def componentParsers: List[PBParser[_]] = Nil
  }
  implicit def repeatedParser[A](implicit parser: PBParser[A]): PBParser[List[A]] =
    possiblyRepeatedWithParser(parser) { values: List[A] => values.reverse }
  implicit def optionalParser[A](implicit parser: PBParser[A]): PBParser[Option[A]] =
    possiblyRepeatedWithParser(parser) { values: List[A] => values.headOption }
  implicit def mapParser[K, V](implicit parser: PBParser[(K, V)]): PBParser[Map[K, V]] =
    possiblyRepeatedWithParser(parser) { values: List[(K, V)] => values.toMap }
  implicit def collectionMapParser[K, V](implicit parser: PBParser[(K, V)]): PBParser[collection.Map[K, V]] =
    possiblyRepeatedWithParser(parser) { values: List[(K, V)] => values.toMap }
  implicit def seqParser[A](implicit parser: PBParser[A]): PBParser[Seq[A]] =
    possiblyRepeatedWithParser(parser) { values: List[A] => values.reverse }
}

object PBParser extends PBParserImplicits {
  def apply[A : PBParser]: PBParser[A] = implicitly
  
  implicit def BooleanParser$: PBParser[Boolean] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readBool()
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit def ByteParser$: PBParser[Byte] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readInt32().toByte
  }
  // Stored as variants, but larger in memory: https://groups.google.com/forum/#!topic/protobuf/Er39mNGnRWU
  implicit def ShortParser$: PBParser[Short] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readInt32().toShort
  }
  implicit def IntParser$: PBParser[Int] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readInt32()
  }
  implicit def LongParser$: PBParser[Long] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readInt64()
  }
  implicit def FloatParser$: PBParser[Float] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readFloat()
  }
  implicit def DoubleParser$: PBParser[Double] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readDouble()
  }
  implicit def StringParser$: PBParser[String] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readString()
  }
  implicit def BytesParser$: PBParser[Array[Byte]] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    input.readByteArray()
  }
  implicit def CNilParser$: PBParser[CNil] = exactlyOnce { (input: CodedInputStream, topLevel: Boolean) =>
    throw new UnsupportedOperationException("Can't read CNil")
  }
  
  implicit object FunctorParser extends Functor[PBParser] {
    override def map[A, B](parser: PBParser[A])(f: A => B): PBParser[B] = new PBParser[B] {
      override def readFieldWithoutTag(input: CodedInputStream, isTopLevel: Boolean): Unit =
        parser.readFieldWithoutTag(input, isTopLevel)
      override def build: B = f(parser.build)
    }
  }
}
