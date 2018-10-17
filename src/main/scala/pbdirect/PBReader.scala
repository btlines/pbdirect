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

  implicit val functorReader: Functor[PBReader] = new Functor[PBReader] {
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

  implicit val cnilParser: PBParser[CNil] = instance {
    (index: Int, bytes: Array[Byte]) =>
      throw new UnsupportedOperationException("Can't read CNil")
  }
}

trait PBConsParser extends LowPriorityPBParserImplicits {
  implicit def consParser[H, T <: HList](
    implicit head: PBParser[H], tail: Lazy[PBParser[T]]
  ): PBParser[H :: T] =
    instance { (index: Int, bytes: Array[Byte]) =>
      head.parse(index, bytes) :: tail.value.parse(index+1, bytes)
    }

  implicit def cconsParser[H, T <: Coproduct](
    implicit head: PBParser[H], tail: Lazy[PBParser[T]]
  ): PBParser[H :+: T] =
    instance { (index: Int, bytes: Array[Byte]) =>
      Try {
        Inl(head.parse(index, bytes))
      } getOrElse {
        Inr(tail.value.parse(index, bytes))
      }
    }
}

trait PBConsParser2 extends PBConsParser {
  implicit def consParser2[H1, H2, T <: HList](
    implicit h1: PBParser[H1], h2: PBParser[H2], tail: Lazy[PBParser[T]]
  ): PBParser[H1 :: H2 :: T] =
    instance { (index: Int, bytes: Array[Byte]) =>
      h1.parse(index, bytes) :: h2.parse(index+1, bytes) :: tail.value.parse(index+2, bytes)
    }

  implicit def cconsParser2[H1, H2, T <: Coproduct](
    implicit h1: PBParser[H1], h2: PBParser[H2], tail: Lazy[PBParser[T]]
  ): PBParser[H1 :+: H2 :+: T] =
    instance { (index: Int, bytes: Array[Byte]) =>
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
    instance { (index: Int, bytes: Array[Byte]) =>
      h1.parse(index, bytes) ::
        h2.parse(index+1, bytes) ::
        h3.parse(index+2, bytes) ::
        h4.parse(index+3, bytes) ::
        tail.value.parse(index+4, bytes)
    }

  implicit def cconsParser4[H1, H2, H3, H4, T <: Coproduct](
    implicit
    h1: PBParser[H1],
    h2: PBParser[H2],
    h3: PBParser[H3],
    h4: PBParser[H4],
    tail: Lazy[PBParser[T]]
  ): PBParser[H1 :+: H2 :+: H3 :+: H4 :+: T] =
    instance { (index: Int, bytes: Array[Byte]) =>
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
  implicit def consParser4[H1, H2, H3, H4, H5, H6, H7, H8, T <: HList](
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
    instance { (index: Int, bytes: Array[Byte]) =>
      h1.parse(index, bytes) ::
        h2.parse(index+1, bytes) ::
        h3.parse(index+2, bytes) ::
        h4.parse(index+3, bytes) ::
        h5.parse(index+4, bytes) ::
        h6.parse(index+5, bytes) ::
        h7.parse(index+6, bytes) ::
        h8.parse(index+7, bytes) ::
        tail.value.parse(index+8, bytes)
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
    instance { (index: Int, bytes: Array[Byte]) =>
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
  implicit def consParser4[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16, T <: HList](
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
  ): PBParser[H1 :: H2 :: H3 :: H4 :: H5 :: H6 :: H7 :: H8 :: H9 :: H10 :: H11 :: H12 :: H13 :: H14 :: H15 :: H16 :: T] =
    instance { (index: Int, bytes: Array[Byte]) =>
      h1.parse(index, bytes) ::
        h2.parse(index+1, bytes) ::
        h3.parse(index+2, bytes) ::
        h4.parse(index+3, bytes) ::
        h5.parse(index+4, bytes) ::
        h6.parse(index+5, bytes) ::
        h7.parse(index+6, bytes) ::
        h8.parse(index+7, bytes) ::
        h9.parse(index+8, bytes) ::
        h10.parse(index+9, bytes) ::
        h11.parse(index+10, bytes) ::
        h12.parse(index+11, bytes) ::
        h13.parse(index+12, bytes) ::
        h14.parse(index+13, bytes) ::
        h15.parse(index+14, bytes) ::
        h16.parse(index+15, bytes) ::
        tail.value.parse(index+16, bytes)
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
  ): PBParser[H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: H9 :+: H10 :+: H11 :+: H12 :+: H13 :+: H14 :+: H15 :+: H16 :+: T] =
    instance { (index: Int, bytes: Array[Byte]) =>
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
}

object PBParser extends PBParserImplicits
