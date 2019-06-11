package pbdirect

import java.io.ByteArrayOutputStream

import cats.data.{NonEmptyList => NEL}
import cats.Functor
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import shapeless.{:+:, ::, Annotations, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}
import shapeless.ops.hlist.ToList

import scala.util.Try

trait PBReader[A] {
  def read(input: CodedInputStream): A
}

trait LowerPriorityPBReaderImplicits {

  def instance[A](f: CodedInputStream => A): PBReader[A] =
    new PBReader[A] {
      override def read(input: CodedInputStream): A = f(input)
    }
  implicit def coprodReader[A, R <: Coproduct](
    implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]]
  ): PBReader[A] = instance { (input: CodedInputStream) =>
    val bytes = input.readByteArray()

    // wraps the bytes into a protobuf single field message
    val out = new ByteArrayOutputStream()
    val pbOut = CodedOutputStream.newInstance(out)
    pbOut.writeByteArray(1, bytes)
    pbOut.flush()
    gen.from(repr.value.parse(NEL.one(1), out.toByteArray))
  }
}

trait PBReaderImplicits extends LowerPriorityPBReaderImplicits {

  implicit def prodReader[A, R <: HList, I <: HList](
    implicit
    gen: Generic.Aux[A, R],
    repr: Lazy[PBParser[R]],
    annotations: Annotations.Aux[Index, A, I],
    toList: ToList[I, Option[Index]]
  ): PBReader[A] = instance { (input: CodedInputStream) =>
    val bytes = input.readByteArray()
    val annotationList = toList(annotations())
    val index = NEL
      .fromList(
        (1 to annotationList.size).toList.zip(annotationList).map {
          case (i, None)           => i
          case (_, Some(Index(i))) => i
        }
      )
      .getOrElse(NEL.one(1))

    gen.from(repr.value.parse(index, bytes))
  }

  implicit def enumReader[A](
    implicit
    values: Enum.Values[A],
    ordering: Ordering[A],
    reader: PBReader[Int]
  ): PBReader[A] = instance { (input: CodedInputStream) =>
    Enum.fromInt[A](reader.read(input))
  }
  implicit def enumerationReader[E <: Enumeration](
    implicit
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

  def apply[A: PBReader]: PBReader[A] = implicitly

  implicit val functorReader: Functor[PBReader] = new Functor[PBReader] {
    override def map[A, B](reader: PBReader[A])(f: A => B): PBReader[B] = instance { (input: CodedInputStream) =>
      f(reader.read(input))
    }
  }
}

trait PBParser[A] {
  def parse(index: NEL[Int], bytes: Array[Byte]): A
}

trait LowPriorityPBParserImplicits {

  def instance[A](f: (NEL[Int], Array[Byte]) => A): PBParser[A] = new PBParser[A] {
    override def parse(index: NEL[Int], bytes: Array[Byte]): A = f(index, bytes)
  }
  implicit val hnilParser: PBParser[HNil] = instance { (index: NEL[Int], bytes: Array[Byte]) =>
    HNil
  }

  implicit val cnilParser: PBParser[CNil] = instance { (index: NEL[Int], bytes: Array[Byte]) =>
    throw new UnsupportedOperationException("Can't read CNil")
  }
}

trait PBConsParser extends LowPriorityPBParserImplicits {
  implicit def consParser[H, T <: HList](
    implicit head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :: T] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      val remainingIndex = NEL.fromList(index.tail).getOrElse(NEL.one(1))
      head.parse(index, bytes) :: tail.value.parse(remainingIndex, bytes)
    }

  implicit def cconsParser[H, T <: Coproduct](
    implicit head: PBParser[H],
    tail: Lazy[PBParser[T]]
  ): PBParser[H :+: T] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      Try {
        Inl(head.parse(index, bytes))
      } getOrElse {
        Inr(tail.value.parse(index, bytes))
      }
    }
}

trait PBConsParser2 extends PBConsParser {
  implicit def consParser2[H1, H2, T <: HList](
    implicit h1: PBParser[H1],
    h2: PBParser[H2],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :: H2 :: T] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      val index1 = NEL.fromList(index.tail).getOrElse(NEL.one(1))
      val index2 = NEL.fromList(index1.tail).getOrElse(NEL.one(1))
      h1.parse(index, bytes) :: h2.parse(index1, bytes) :: tail.value.parse(index2, bytes)
    }

  implicit def cconsParser2[H1, H2, T <: Coproduct](
    implicit h1: PBParser[H1],
    h2: PBParser[H2],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :+: H2 :+: T] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      Try {
        Inl(h1.parse(index, bytes))
      } orElse Try {
        Inr(Inl(h2.parse(index, bytes)))
      } getOrElse {
        Inr(Inr(tail.value.parse(index, bytes)))
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
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      val index1 = NEL.fromList(index.tail).getOrElse(NEL.one(1))
      val index2 = NEL.fromList(index1.tail).getOrElse(NEL.one(1))
      val index3 = NEL.fromList(index2.tail).getOrElse(NEL.one(1))
      val index4 = NEL.fromList(index3.tail).getOrElse(NEL.one(1))
      h1.parse(index, bytes) ::
      h2.parse(index1, bytes) ::
      h3.parse(index2, bytes) ::
      h4.parse(index3, bytes) ::
      tail.value.parse(index4, bytes)
    }

  implicit def cconsParser4[H1, H2, H3, H4, T <: Coproduct](
    implicit
    h1: PBParser[H1],
    h2: PBParser[H2],
    h3: PBParser[H3],
    h4: PBParser[H4],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :+: H2 :+: H3 :+: H4 :+: T] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      Try {
        Inl(h1.parse(index, bytes))
      } orElse Try {
        Inr(Inl(h2.parse(index, bytes)))
      } orElse Try {
        Inr(Inr(Inl(h3.parse(index, bytes))))
      } orElse Try {
        Inr(Inr(Inr(Inl(h4.parse(index, bytes)))))
      } getOrElse {
        Inr(Inr(Inr(Inr(tail.value.parse(index, bytes)))))
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
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      val index1 = NEL.fromList(index.tail).getOrElse(NEL.one(1))
      val index2 = NEL.fromList(index1.tail).getOrElse(NEL.one(1))
      val index3 = NEL.fromList(index2.tail).getOrElse(NEL.one(1))
      val index4 = NEL.fromList(index3.tail).getOrElse(NEL.one(1))
      val index5 = NEL.fromList(index4.tail).getOrElse(NEL.one(1))
      val index6 = NEL.fromList(index5.tail).getOrElse(NEL.one(1))
      val index7 = NEL.fromList(index6.tail).getOrElse(NEL.one(1))
      val index8 = NEL.fromList(index7.tail).getOrElse(NEL.one(1))
      h1.parse(index, bytes) ::
      h2.parse(index1, bytes) ::
      h3.parse(index2, bytes) ::
      h4.parse(index3, bytes) ::
      h5.parse(index4, bytes) ::
      h6.parse(index5, bytes) ::
      h7.parse(index6, bytes) ::
      h8.parse(index7, bytes) ::
      tail.value.parse(index8, bytes)
    }

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
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      Try {
        Inl(h1.parse(index, bytes))
      } orElse Try {
        Inr(Inl(h2.parse(index, bytes)))
      } orElse Try {
        Inr(Inr(Inl(h3.parse(index, bytes))))
      } orElse Try {
        Inr(Inr(Inr(Inl(h4.parse(index, bytes)))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inl(h5.parse(index, bytes))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inl(h6.parse(index, bytes)))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inl(h7.parse(index, bytes))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h8.parse(index, bytes)))))))))
      } getOrElse {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(tail.value.parse(index, bytes)))))))))
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
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      val index1 = NEL.fromList(index.tail).getOrElse(NEL.one(1))
      val index2 = NEL.fromList(index1.tail).getOrElse(NEL.one(1))
      val index3 = NEL.fromList(index2.tail).getOrElse(NEL.one(1))
      val index4 = NEL.fromList(index3.tail).getOrElse(NEL.one(1))
      val index5 = NEL.fromList(index4.tail).getOrElse(NEL.one(1))
      val index6 = NEL.fromList(index5.tail).getOrElse(NEL.one(1))
      val index7 = NEL.fromList(index6.tail).getOrElse(NEL.one(1))
      val index8 = NEL.fromList(index7.tail).getOrElse(NEL.one(1))
      val index9 = NEL.fromList(index8.tail).getOrElse(NEL.one(1))
      val index10 = NEL.fromList(index9.tail).getOrElse(NEL.one(1))
      val index11 = NEL.fromList(index10.tail).getOrElse(NEL.one(1))
      val index12 = NEL.fromList(index11.tail).getOrElse(NEL.one(1))
      val index13 = NEL.fromList(index12.tail).getOrElse(NEL.one(1))
      val index14 = NEL.fromList(index13.tail).getOrElse(NEL.one(1))
      val index15 = NEL.fromList(index14.tail).getOrElse(NEL.one(1))
      val index16 = NEL.fromList(index15.tail).getOrElse(NEL.one(1))
      h1.parse(index, bytes) ::
      h2.parse(index1, bytes) ::
      h3.parse(index2, bytes) ::
      h4.parse(index3, bytes) ::
      h5.parse(index4, bytes) ::
      h6.parse(index5, bytes) ::
      h7.parse(index6, bytes) ::
      h8.parse(index7, bytes) ::
      h9.parse(index8, bytes) ::
      h10.parse(index9, bytes) ::
      h11.parse(index10, bytes) ::
      h12.parse(index11, bytes) ::
      h13.parse(index12, bytes) ::
      h14.parse(index13, bytes) ::
      h15.parse(index14, bytes) ::
      h16.parse(index15, bytes) ::
      tail.value.parse(index16, bytes)
    }

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
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      Try {
        Inl(h1.parse(index, bytes))
      } orElse Try {
        Inr(Inl(h2.parse(index, bytes)))
      } orElse Try {
        Inr(Inr(Inl(h3.parse(index, bytes))))
      } orElse Try {
        Inr(Inr(Inr(Inl(h4.parse(index, bytes)))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inl(h5.parse(index, bytes))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inl(h6.parse(index, bytes)))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inl(h7.parse(index, bytes))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h8.parse(index, bytes)))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h9.parse(index, bytes))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h10.parse(index, bytes)))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h11.parse(index, bytes))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h12.parse(index, bytes)))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h13.parse(index, bytes))))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h14.parse(index, bytes)))))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h15.parse(index, bytes))))))))))))))))
      } orElse Try {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(h16.parse(index, bytes)))))))))))))))))
      } getOrElse {
        Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(tail.value.parse(index, bytes)))))))))))))))))
      }
    }
}

trait PBParserImplicits extends PBConsParser16 {
  implicit def repeatedParser[A](implicit reader: PBReader[A]): PBParser[List[A]] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      val input = CodedInputStream.newInstance(bytes)
      var done = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                               => done = true
          case tag if (tag >> 3) == index.head => as ::= reader.read(input)
          case tag                             => input.skipField(tag)
        }
      }
      as.reverse
    }
  implicit def requiredParser[A](implicit reader: PBReader[A]): PBParser[A] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      val input = CodedInputStream.newInstance(bytes)
      var done = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                               => done = true
          case tag if (tag >> 3) == index.head => as ::= reader.read(input)
          case tag                             => input.skipField(tag)
        }
      }
      as.head
    }
  implicit def optionalParser[A](implicit parser: PBParser[List[A]]): PBParser[Option[A]] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      parser.parse(index, bytes).lastOption
    }
  implicit def mapParser[K, V](implicit parser: PBParser[List[(K, V)]]): PBParser[Map[K, V]] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      parser.parse(index, bytes).toMap
    }
  implicit def collectionMapParser[K, V](implicit parser: PBParser[List[(K, V)]]): PBParser[collection.Map[K, V]] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      parser.parse(index, bytes).toMap
    }
  implicit def seqParser[A](implicit parser: PBParser[List[A]]): PBParser[Seq[A]] =
    instance { (index: NEL[Int], bytes: Array[Byte]) =>
      parser.parse(index, bytes)
    }
}

object PBParser extends PBParserImplicits
