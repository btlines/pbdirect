package pbdirect

import cats.Functor
import com.google.protobuf.CodedInputStream
import cats.data.{NonEmptyList => NEL}
import shapeless.{:+:, ::, Annotations, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}
import shapeless.ops.hlist.ToList

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scala.util.Try

/** Reads data for a single Protobuf message.  When read() is called, it is
  * assumed that the caller has already consumed the tag for this message.
  */
trait PBReader[A] {
  def read(input: CodedInputStream, size: Option[Int]): A
}

/** Reads data for a single field type in a Product structure, or a whole
  * Protobuf message.  The caller should instantiate a PBParser and then call
  * readFieldWithoutTag once for each copy of the field to be read, after
  * first consuming the field's tag from the CodedInputStream (or doing
  * nothing, in the case of a whole Protobuf message).  Once all copies of the
  * field are read, call build() exactly once to build the resulting collection
  * of field values.
  * 
  * The type of collection produced by build(), and whether that collection
  * contains all copies of the field or only a subset, is left to the
  * implementation.  For example, for a repeated field, all copies should
  * be kept in the order they were read.
  */
trait PBParser[A] {
  def readSingleFieldAndBuild(input: CodedInputStream, size: Option[Int]): A = {
    readFieldWithoutTag(NEL.one(1), input, size)
    build()
  }
  private[pbdirect] def readFieldWithoutTag(index: NEL[Int], input: CodedInputStream, size: Option[Int]): Unit
  private[pbdirect] def build(): A
  private[pbdirect] def componentParsers: List[PBParser[_]] = List(this)
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
  
  implicit def prodReader[A, R <: HList, I <: HList](implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]],
    annotations: Annotations.Aux[Index, A, I],
    toList: ToList[I, Option[Index]]
  ): PBReader[A] = instance { (input: CodedInputStream, size: Option[Int]) =>
    val annotationList = toList(annotations())
    val index = NEL
    .fromList(
      (1 to annotationList.size).toList.zip(annotationList).map {
        case (i, None)           => i
        case (_, Some(Index(i))) => i
      }
    )
    .getOrElse(NEL.one(1))
    repr.value.readFieldWithoutTag(index, input, size=size)
    gen.from(repr.value.build())
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
    private[pbdirect] override def readFieldWithoutTag(index: NEL[Int], input: CodedInputStream, size: Option[Int]): Unit = {
      value = Some(readF(input, size))
    }
    private[pbdirect] override def build(): A = {
      val v = value.get
      value = None
      v
    }
  }
  
  def possiblyRepeatedParser[A, B](sizeHint: Int)(readFieldWithoutTagF: (CodedInputStream, Option[Int]) => A)(builder: mutable.IndexedSeq[A] => B): PBParser[B] = new PBParser[B] {
    private var values: mutable.ArrayBuffer[A] = null
    private[pbdirect] override def readFieldWithoutTag(index: NEL[Int], input: CodedInputStream, size: Option[Int]): Unit = {
      if (values == null) values = new ArrayBuffer[A](sizeHint)
      values += readFieldWithoutTagF(input, size)
    }
    private[pbdirect] override def build(): B = {
      if (values == null) values = new ArrayBuffer[A](initialSize=0)
      val v = builder(values)
      values = null
      v
    }
  }
  def possiblyRepeatedParserWithReader[A, B](sizeHint: Int)(reader: PBReader[A])(builder: mutable.IndexedSeq[A] => B): PBParser[B] =
    possiblyRepeatedParser(sizeHint)(reader.read)(builder)
  
  implicit val hnilParser: PBParser[HNil] = new PBParser[HNil] {
    private[pbdirect] override def readFieldWithoutTag(index: NEL[Int], input: CodedInputStream, size: Option[Int]): Unit = ()
    private[pbdirect] override def build(): HNil = HNil
    private[pbdirect] override def componentParsers: List[PBParser[_]] = Nil
  }
  implicit val cnilParser: PBParser[CNil] = new PBParser[CNil] {
    private[pbdirect] override def readFieldWithoutTag(index: NEL[Int], input: CodedInputStream, size: Option[Int]): Unit =
      throw new UnsupportedOperationException("Can't read HNil")
    private[pbdirect] override def build(): CNil = throw new UnsupportedOperationException("Can't build CNil")
    private[pbdirect] override def componentParsers: List[PBParser[_]] = Nil
  }
}

trait PBConsParser extends LowPriorityPBParserImplicits {
  def consParserInstance[V](
    componentParsersList: List[PBParser[_]],
    buildF: () => V
  ): PBParser[V] = new PBParser[V] {
    private[pbdirect] override val componentParsers: List[PBParser[_]] = componentParsersList
  
    private[pbdirect] override def readFieldWithoutTag(index: NEL[Int], input: CodedInputStream, size: Option[Int]): Unit = {
      val sizeLimit = size match {
        // Our parent provided our size, which is a signal that the size field
        // is not in the stream.
        case Some(sz) => sz
        // Our parent was unable to provide a size for us, which is a signal
        // that the size field must still be available in the stream.
        case None => input.readRawVarint32()
      }
  
      val oldLimit = input.pushLimit(sizeLimit)
  
      val componentParsersByIndex: Map[Int, PBParser[_]] = index.toList.zip(componentParsers).toMap
  
      var done = false
      while (!done) {
        val tag = input.readTag()
        val oneBasedIndex = tag >> 3
        if (tag == 0) {
          done = true
        } else {
          componentParsersByIndex.get(oneBasedIndex) match {
            case Some(parser) => parser.readFieldWithoutTag(NEL.one(1), input, size=None)
            case None => input.skipField(tag)
          }
        }
      }
  
      input.popLimit(oldLimit)
    }
  
    private[pbdirect] override def build(): V = buildF()
  }
  
  def readCconsBytes(input: CodedInputStream, size: Option[Int]): Array[Byte] = {
    // Unfortunately (for efficiency, at least), we must read the input as a
    // byte array here, because we have to interpret it potentially multiple
    // times.
    size match {
      // Our parent provided our size, which is a signal that the size field
      // is not in the stream.
      case Some(sz) => input.readRawBytes(sz)
      // Our parent was unable to provide a size for us, which is a signal
      // that the size field must still be available in the stream.
      case None => input.readByteArray()
    }
  }
  
  implicit def consParser[H, T <: HList](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :: T] = consParserInstance(
    head :: tail.value.componentParsers,
    { () => head.build :: tail.value.build }
  )
  
  implicit def cconsParser[H, T <: Coproduct](implicit
    head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :+: T] = exactlyOnceParser { (input: CodedInputStream, size: Option[Int]) =>
    val bytes = readCconsBytes(input, size)
    Try {
      Inl(head.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))
    } getOrElse {
      Inr(tail.value.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))
    }
  }
}

trait PBConsParser2 extends PBConsParser {
  implicit def consParser2[H1, H2, T <: HList](
    implicit h1: PBParser[H1],
    h2: PBParser[H2],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :: H2 :: T] =
    consParserInstance(
      h1 :: h2 :: tail.value.componentParsers,
      { () => h1.build :: h2.build :: tail.value.build }
    )

  implicit def cconsParser2[H1, H2, T <: Coproduct](
    implicit h1: PBParser[H1],
    h2: PBParser[H2],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :+: H2 :+: T] = exactlyOnceParser { (input: CodedInputStream, size: Option[Int]) =>
    val bytes = readCconsBytes(input, size)
    Try {
      Inl(h1.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))
    } orElse Try {
      Inr(Inl(h2.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))
    } getOrElse {
      Inr(Inr(tail.value.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))
    }
  }
}

trait PBConsParser4 extends PBConsParser2 {
  implicit def consParser4[H1, H2, H3, H4, T <: HList](
    implicit
    h1: PBParser[H1],
    h2: PBParser[H2],
    h3: PBParser[H3],
    h4: PBParser[H4],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :: H2 :: H3 :: H4 :: T] =
    consParserInstance(
      h1 :: h2 :: h3 :: h4 :: tail.value.componentParsers,
      { () => h1.build :: h2.build :: h3.build :: h4.build :: tail.value.build }
    )

  implicit def cconsParser4[H1, H2, H3, H4, T <: Coproduct](
    implicit
    h1: PBParser[H1],
    h2: PBParser[H2],
    h3: PBParser[H3],
    h4: PBParser[H4],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :+: H2 :+: H3 :+: H4 :+: T] = exactlyOnceParser { (input: CodedInputStream, size: Option[Int]) =>
    val bytes = readCconsBytes(input, size)
    Try {
      Inl(h1.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))
    } orElse Try {
      Inr(Inl(h2.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))
    } orElse Try {
      Inr(Inr(Inl(h3.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))
    } orElse Try {
      Inr(Inr(Inr(Inl(h4.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))
    } getOrElse {
      Inr(Inr(Inr(Inr(tail.value.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))
    }
  }
}

trait PBConsParser8 extends PBConsParser4 {
  implicit def consParser8[H1, H2, H3, H4, H5, H6, H7, H8, T <: HList](
    implicit
    h1: PBParser[H1],
    h2: PBParser[H2],
    h3: PBParser[H3],
    h4: PBParser[H4],
    h5: PBParser[H5],
    h6: PBParser[H6],
    h7: PBParser[H7],
    h8: PBParser[H8],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :: H2 :: H3 :: H4 :: H5 :: H6 :: H7 :: H8 :: T] =
    consParserInstance(
      h1 :: h2 :: h3 :: h4 :: h5 :: h6 :: h7 :: h8 :: tail.value.componentParsers,
      { () =>
        h1.build :: h2.build :: h3.build :: h4.build ::
        h5.build :: h6.build :: h7.build :: h8.build :: 
        tail.value.build
      }
    )

  implicit def cconsParser8[H1, H2, H3, H4, H5, H6, H7, H8, T <: Coproduct](
    implicit
    h1: PBParser[H1],
    h2: PBParser[H2],
    h3: PBParser[H3],
    h4: PBParser[H4],
    h5: PBParser[H5],
    h6: PBParser[H6],
    h7: PBParser[H7],
    h8: PBParser[H8],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: T] =
    exactlyOnceParser { (input: CodedInputStream, size: Option[Int]) =>
      val bytes = readCconsBytes(input, size)
      Try {
        Inl(h1.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))
      } orElse Try {
        Inr(Inl(h2.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))
      } orElse Try {
        Inr(Inr(Inl(h3.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))
      } orElse Try {
        Inr(Inr(Inr(Inl(h4.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inl(h5.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inl(h6.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inl(h7.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h8.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))))
      } getOrElse {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(tail.value.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))))
      }
    }
}

trait PBConsParser16 extends PBConsParser8 {
  implicit def consParser16[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16, T <: HList](
    implicit
    h1: PBParser[H1],
    h2: PBParser[H2],
    h3: PBParser[H3],
    h4: PBParser[H4],
    h5: PBParser[H5],
    h6: PBParser[H6],
    h7: PBParser[H7],
    h8: PBParser[H8],
    h9: PBParser[H9],
    h10: PBParser[H10],
    h11: PBParser[H11],
    h12: PBParser[H12],
    h13: PBParser[H13],
    h14: PBParser[H14],
    h15: PBParser[H15],
    h16: PBParser[H16],
    tail: Lazy[PBParser[T]]
  ): PBParser[
  H1 :: H2 :: H3 :: H4 :: H5 :: H6 :: H7 :: H8 :: H9 :: H10 :: H11 :: H12 :: H13 :: H14 :: H15 :: H16 :: T
  ] =
    consParserInstance(
      h1 :: h2 :: h3 :: h4 :: h5 :: h6 :: h7 :: h8 ::
      h9 :: h10 :: h11 :: h12 :: h13 :: h14 :: h15 :: h16 ::
      tail.value.componentParsers,
      { () =>
        h1.build :: h2.build :: h3.build :: h4.build ::
        h5.build :: h6.build :: h7.build :: h8.build ::
        h9.build :: h10.build :: h11.build :: h12.build ::
        h13.build :: h14.build :: h15.build :: h16.build ::
        tail.value.build
      }
    )

  implicit def cconsParser16[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16, T <: Coproduct](
    implicit
    h1: PBParser[H1],
    h2: PBParser[H2],
    h3: PBParser[H3],
    h4: PBParser[H4],
    h5: PBParser[H5],
    h6: PBParser[H6],
    h7: PBParser[H7],
    h8: PBParser[H8],
    h9: PBParser[H9],
    h10: PBParser[H10],
    h11: PBParser[H11],
    h12: PBParser[H12],
    h13: PBParser[H13],
    h14: PBParser[H14],
    h15: PBParser[H15],
    h16: PBParser[H16],
    tail: Lazy[PBParser[T]]
  ): PBParser[
  H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: H9 :+: H10 :+: H11 :+: H12 :+: H13 :+: H14 :+: H15 :+: H16 :+: T
  ] =
    exactlyOnceParser { (input: CodedInputStream, size: Option[Int]) =>
      val bytes = readCconsBytes(input, size)
      Try {
        Inl(h1.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))
      } orElse Try {
        Inr(Inl(h2.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))
      } orElse Try {
        Inr(Inr(Inl(h3.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))
      } orElse Try {
        Inr(Inr(Inr(Inl(h4.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inl(h5.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inl(h6.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inl(h7.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h8.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h9.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h10.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h11.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h12.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h13.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h14.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h15.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length)))))))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h16.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))))))))))))
      } getOrElse {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(tail.value.readSingleFieldAndBuild(CodedInputStream.newInstance(bytes), size=Some(bytes.length))))))))))))))))))
      }
    }
}

trait PBParserImplicits extends PBConsParser16 {
  private val SINGLETON_SIZE_HINT = 1
  private val COLLECTION_SIZE_HINT = 16
  
  implicit def repeatedParser[A](implicit reader: PBReader[A]): PBParser[List[A]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values.toList }
  implicit def requiredParser[A](implicit reader: PBReader[A]): PBParser[A] =
    exactlyOnceParser(reader.read)
  implicit def optionalParser[A](implicit reader: PBReader[A]): PBParser[Option[A]] =
    possiblyRepeatedParserWithReader(sizeHint=SINGLETON_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values.lastOption }
  implicit def mapParser[K, V](implicit reader: PBReader[(K, V)]): PBParser[Map[K, V]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[(K, V)] => values.toMap }
  implicit def collectionMapParser[K, V](implicit reader: PBReader[(K, V)]): PBParser[collection.Map[K, V]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[(K, V)] => values.toMap }
  implicit def seqParser[A](implicit reader: PBReader[A]): PBParser[collection.Seq[A]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values }
  implicit def immutableSeqParser[A](implicit reader: PBReader[A]): PBParser[immutable.Seq[A]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values.toIndexedSeq }
  implicit def indexedSeqParser[A](implicit reader: PBReader[A]): PBParser[collection.IndexedSeq[A]] =
    possiblyRepeatedParserWithReader(sizeHint=COLLECTION_SIZE_HINT)(reader) { values: mutable.IndexedSeq[A] => values }
}

object PBParser extends PBParserImplicits
