package pbdirect

import java.io.ByteArrayOutputStream

import cats.data.{NonEmptyList => NEL}
import cats.{ Contravariant, Functor }
import com.google.protobuf.CodedOutputStream
import shapeless.{:+:, ::, Annotations, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}
import shapeless.ops.hlist.ToList
import scala.collection.GenMap

trait PBWriter[A] {
  def writeTo(index: NEL[Int], value: A, out: CodedOutputStream): Unit
}

trait LowPriorityPBWriterImplicits {

  def instance[A](f: (NEL[Int], A, CodedOutputStream) => Unit): PBWriter[A] =
    new PBWriter[A] {
      override def writeTo(index: NEL[Int], value: A, out: CodedOutputStream): Unit = f(index, value, out)
    }

  implicit val hnilWriter: PBWriter[HNil] = instance {
    (_: NEL[Int], _: HNil, _: CodedOutputStream) => ()
  }

  implicit def prodWriter[A, R <: HList, I <: HList](
    implicit gen: Generic.Aux[A, R],
    annotations: Annotations.Aux[Index, A, I],
    toList: ToList[I, Option[Index]],
    writer: Lazy[PBWriter[R]]
  ): PBWriter[A] =
    instance { (index: NEL[Int], value: A, out: CodedOutputStream) =>
      val buffer = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(buffer)
      val annotationList = toList(annotations())
      val fields = NEL.fromList(
        (1 to annotationList.size).toList.zip(annotationList).map {
        case (i, None) => i
        case (_, Some(Index(i))) => i
      }).getOrElse(NEL.one(1))

      writer.value.writeTo(fields, gen.to(value), pbOut)
      pbOut.flush()
      out.writeByteArray(index.head, buffer.toByteArray)
    }

  implicit val cnilWriter: PBWriter[CNil] = instance {
    (_: NEL[Int], _: CNil, _: CodedOutputStream) => throw new Exception("Can't write CNil")
  }
  implicit def coprodWriter[A, R <: Coproduct](implicit gen: Generic.Aux[A, R], writer: PBWriter[R]): PBWriter[A] =
    instance { (index: NEL[Int], value: A, out: CodedOutputStream) =>
      writer.writeTo(index, gen.to(value), out)
    }
}

trait PBConsWriter extends LowPriorityPBWriterImplicits {
  implicit def consWriter[H, T <: HList](implicit head: PBWriter[H], tail: Lazy[PBWriter[T]]): PBWriter[H :: T] =
    instance { (index: NEL[Int], value: H :: T, out: CodedOutputStream) =>
        head.writeTo(index, value.head, out)
        NEL.fromList(index.tail).foreach(tail.value.writeTo(_, value.tail, out))
    }

  implicit def cconsWriter[H, T <: Coproduct](implicit head: PBWriter[H], tail: PBWriter[T]): PBWriter[H :+: T] =
    instance { (index: NEL[Int], value: H :+: T, out: CodedOutputStream) =>
      value match {
        case Inl(v) => head.writeTo(index, v, out)
        case Inr(v) => tail.writeTo(index, v, out)
      }
    }
}

trait PBConsWriter2 extends PBConsWriter {
  implicit def consWriter2[H1, H2, T <: HList](
    implicit h1: PBWriter[H1], h2: PBWriter[H2], tail: Lazy[PBWriter[T]]
  ): PBWriter[H1 :: H2 :: T] =
    instance { (index: NEL[Int], value: H1 :: H2 :: T, out: CodedOutputStream) =>
      h1.writeTo(index, value.head, out)
      NEL.fromList(index.tail).foreach(h2.writeTo(_, value.tail.head, out))
      NEL.fromList(index.tail.tail).foreach(tail.value.writeTo(_, value.tail.tail, out))
    }

  implicit def cconsWriter2[H1, H2, T <: Coproduct](
    implicit h1: PBWriter[H1], h2: PBWriter[H2], tail: PBWriter[T]): PBWriter[H1 :+: H2 :+: T] =
    instance { (index: NEL[Int], value: H1 :+: H2 :+: T, out: CodedOutputStream) =>
      value match {
        case Inl(v) => h1.writeTo(index, v, out)
        case Inr(Inl(v)) => h2.writeTo(index, v, out)
        case Inr(Inr(v)) => tail.writeTo(index, v, out)
      }
    }
}

trait PBConsWriter4 extends PBConsWriter2 {
  implicit def consWriter4[H1, H2, H3, H4, T <: HList](
    implicit
    h1: PBWriter[H1],
    h2: PBWriter[H2],
    h3: PBWriter[H3],
    h4: PBWriter[H4],
    tail: Lazy[PBWriter[T]]
  ): PBWriter[H1 :: H2 :: H3 :: H4 :: T] =
    instance {
      (
        index: NEL[Int],
        value: H1 :: H2 :: H3 :: H4 :: T,
        out: CodedOutputStream
      ) =>
        h1.writeTo(index, value.head, out)
        NEL.fromList(index.tail).foreach(h2.writeTo(_, value.tail.head, out))
        NEL.fromList(index.tail.tail).foreach(h3.writeTo(_, value.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail).foreach(h4.writeTo(_, value.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail).foreach(tail.value.writeTo(_, value.tail.tail.tail.tail, out))
        
    }

  implicit def cconsWriter4[H1, H2, H3, H4, T <: Coproduct](
    implicit
    h1: PBWriter[H1],
    h2: PBWriter[H2],
    h3: PBWriter[H3],
    h4: PBWriter[H4],
    tail: PBWriter[T]
  ): PBWriter[H1 :+: H2 :+: H3 :+: H4 :+: T] =
    instance {
      (
        index: NEL[Int],
        value: H1 :+: H2 :+: H3 :+: H4 :+: T,
        out: CodedOutputStream
      ) =>
        value match {
          case Inl(v) => h1.writeTo(index, v, out)
          case Inr(Inl(v)) => h2.writeTo(index, v, out)
          case Inr(Inr(Inl(v))) => h3.writeTo(index, v, out)
          case Inr(Inr(Inr(Inl(v)))) => h4.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(v)))) => tail.writeTo(index, v, out)
        }
    }
}

trait PBConsWriter8 extends PBConsWriter4 {
  implicit def consWriter8[H1, H2, H3, H4, H5, H6, H7, H8, T <: HList](
    implicit
    h1: PBWriter[H1],
    h2: PBWriter[H2],
    h3: PBWriter[H3],
    h4: PBWriter[H4],
    h5: PBWriter[H5],
    h6: PBWriter[H6],
    h7: PBWriter[H7],
    h8: PBWriter[H8],
    tail: Lazy[PBWriter[T]]
  ): PBWriter[H1 :: H2 :: H3 :: H4 :: H5 :: H6 :: H7 :: H8 :: T] =
    instance {
      (
        index: NEL[Int],
        value: H1 :: H2 :: H3 :: H4 :: H5 :: H6 :: H7 :: H8 :: T,
        out: CodedOutputStream
      ) =>
        h1.writeTo(index, value.head, out)
        NEL.fromList(index.tail).foreach(h2.writeTo(_, value.tail.head, out))
        NEL.fromList(index.tail.tail).foreach(h3.writeTo(_, value.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail).foreach(h4.writeTo(_, value.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail).foreach(h5.writeTo(_, value.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail).foreach(h6.writeTo(_, value.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail).foreach(h7.writeTo(_, value.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail).foreach(h8.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail).foreach(tail.value.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail, out))
    }

  implicit def cconsWriter8[H1, H2, H3, H4, H5, H6, H7, H8, T <: Coproduct](
    implicit
    h1: PBWriter[H1],
    h2: PBWriter[H2],
    h3: PBWriter[H3],
    h4: PBWriter[H4],
    h5: PBWriter[H5],
    h6: PBWriter[H6],
    h7: PBWriter[H7],
    h8: PBWriter[H8],
    tail: PBWriter[T]
  ): PBWriter[H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: T] =
    instance {
      (
        index: NEL[Int],
        value: H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: T,
        out: CodedOutputStream
      ) =>
        value match {
          case Inl(v) => h1.writeTo(index, v, out)
          case Inr(Inl(v)) => h2.writeTo(index, v, out)
          case Inr(Inr(Inl(v))) => h3.writeTo(index, v, out)
          case Inr(Inr(Inr(Inl(v)))) => h4.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inl(v))))) => h5.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inl(v)))))) => h6.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))) => h7.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))) => h8.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(v)))))))) => tail.writeTo(index, v, out)
        }
    }
}

trait PBConsWriter16 extends PBConsWriter8 {
  implicit def consWriter16[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16, T <: HList](
    implicit
    h1: PBWriter[H1],
    h2: PBWriter[H2],
    h3: PBWriter[H3],
    h4: PBWriter[H4],
    h5: PBWriter[H5],
    h6: PBWriter[H6],
    h7: PBWriter[H7],
    h8: PBWriter[H8],
    h9: PBWriter[H9],
    h10: PBWriter[H10],
    h11: PBWriter[H11],
    h12: PBWriter[H12],
    h13: PBWriter[H13],
    h14: PBWriter[H14],
    h15: PBWriter[H15],
    h16: PBWriter[H16],
    tail: Lazy[PBWriter[T]]
  ): PBWriter[H1 :: H2 :: H3 :: H4 :: H5 :: H6 :: H7 :: H8 :: H9 :: H10 :: H11 :: H12 :: H13 :: H14 :: H15 :: H16 :: T] =
    instance {
      (
        index: NEL[Int],
        value: H1 :: H2 :: H3 :: H4 :: H5 :: H6 :: H7 :: H8 :: H9 :: H10 :: H11 :: H12 :: H13 :: H14 :: H15 :: H16 :: T,
        out: CodedOutputStream
      ) =>
        h1.writeTo(index, value.head, out)
        NEL.fromList(index.tail).foreach(h2.writeTo(_, value.tail.head, out))
        NEL.fromList(index.tail.tail).foreach(h3.writeTo(_, value.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail).foreach(h4.writeTo(_, value.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail).foreach(h5.writeTo(_, value.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail).foreach(h6.writeTo(_, value.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail).foreach(h7.writeTo(_, value.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail).foreach(h8.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail).foreach(h9.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail).foreach(h10.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail).foreach(h11.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail).foreach(h12.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail).foreach(h13.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail).foreach(h14.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail).foreach(h15.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail).foreach(h16.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL.fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail).foreach(tail.value.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail, out))
    }

  implicit def cconsWriter16[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16, T <: Coproduct](
     implicit
     h1: PBWriter[H1],
     h2: PBWriter[H2],
     h3: PBWriter[H3],
     h4: PBWriter[H4],
     h5: PBWriter[H5],
     h6: PBWriter[H6],
     h7: PBWriter[H7],
     h8: PBWriter[H8],
     h9: PBWriter[H9],
     h10: PBWriter[H10],
     h11: PBWriter[H11],
     h12: PBWriter[H12],
     h13: PBWriter[H13],
     h14: PBWriter[H14],
     h15: PBWriter[H15],
     h16: PBWriter[H16],
     tail: PBWriter[T]
  ): PBWriter[H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: H9 :+: H10 :+: H11 :+: H12 :+: H13 :+: H14 :+: H15 :+: H16 :+: T] =
    instance {
      (
        index: NEL[Int],
        value: H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: H9 :+: H10 :+: H11 :+: H12 :+: H13 :+: H14 :+: H15 :+: H16 :+: T,
        out: CodedOutputStream
      ) =>
        value match {
          case Inl(v) => h1.writeTo(index, v, out)
          case Inr(Inl(v)) => h2.writeTo(index, v, out)
          case Inr(Inr(Inl(v))) => h3.writeTo(index, v, out)
          case Inr(Inr(Inr(Inl(v)))) => h4.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inl(v))))) => h5.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inl(v)))))) => h6.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))) => h7.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))) => h8.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))))) => h9.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))) => h10.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))))))) => h11.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))))) => h12.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))))))))) => h13.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))))))) => h14.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))))))))))) => h15.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))))))))) => h16.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr((v))))))))))))))))) => tail.writeTo(index, v, out)
        }
    }
}

trait PBWriterImplicits extends PBConsWriter16 {
  implicit object BooleanWriter extends PBWriter[Boolean] {
    override def writeTo(index: NEL[Int], value: Boolean, out: CodedOutputStream): Unit =
      out.writeBool(index.head, value)
  }
  implicit object ByteWriter extends PBWriter[Byte] {
    override def writeTo(index: Int, value: Byte, out: CodedOutputStream): Unit =
      out.writeInt32(index, value)
  }
  implicit object ShortWriter extends PBWriter[Short] {
    override def writeTo(index: Int, value: Short, out: CodedOutputStream): Unit =
      out.writeInt32(index, value)
  }
  implicit object IntWriter extends PBWriter[Int] {
    override def writeTo(index: NEL[Int], value: Int, out: CodedOutputStream): Unit =
      out.writeInt32(index.head, value)
  }
  implicit object LongWriter extends PBWriter[Long] {
    override def writeTo(index: NEL[Int], value: Long, out: CodedOutputStream): Unit =
      out.writeInt64(index.head, value)
  }
  implicit object FloatWriter extends PBWriter[Float] {
    override def writeTo(index: NEL[Int], value: Float, out: CodedOutputStream): Unit =
      out.writeFloat(index.head, value)
  }
  implicit object DoubleWriter extends PBWriter[Double] {
    override def writeTo(index: NEL[Int], value: Double, out: CodedOutputStream): Unit =
      out.writeDouble(index.head, value)
  }
  implicit object StringWriter extends PBWriter[String] {
    override def writeTo(index: NEL[Int], value: String, out: CodedOutputStream): Unit =
      out.writeString(index.head, value)
  }
  implicit object BytesWriter extends PBWriter[Array[Byte]] {
    override def writeTo(index: NEL[Int], value: Array[Byte], out: CodedOutputStream): Unit =
      out.writeByteArray(index.head, value)
  }
  implicit def functorWriter[F[_], A](implicit functor: Functor[F], writer: PBWriter[A]): PBWriter[F[A]] =
    instance { (index: NEL[Int], value: F[A], out: CodedOutputStream) =>
      functor.map(value) { v => writer.writeTo(index, v, out) }
      ()
    }
  implicit def mapWriter[K, V](implicit writer: PBWriter[List[(K, V)]]): PBWriter[Map[K, V]] =
    instance { (index: NEL[Int], value: Map[K, V], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }
  implicit def collectionMapWriter[K, V](implicit writer: PBWriter[List[(K, V)]]): PBWriter[collection.Map[K, V]] =
    instance { (index: Int, value: collection.Map[K, V], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }
  implicit def seqWriter[A](implicit writer: PBWriter[List[A]]): PBWriter[Seq[A]] =
    instance { (index: Int, value: Seq[A], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }
  implicit def enumWriter[E](implicit values: Enum.Values[E], ordering: Ordering[E]): PBWriter[E] =
    instance { (index: NEL[Int], value: E, out: CodedOutputStream) =>
      out.writeInt32(index.head, Enum.toInt(value))
    }
  implicit def enumerationWriter[E <: Enumeration#Value]: PBWriter[E] =
    instance { (index: NEL[Int], value: E, out: CodedOutputStream) =>
      out.writeInt32(index.head, value.id)
    }

  implicit val contravariantWriter: Contravariant[PBWriter] = new Contravariant[PBWriter] {
    override def contramap[A, B](writer: PBWriter[A])(f: B => A): PBWriter[B] =
      instance { (index: NEL[Int], b: B, out: CodedOutputStream) =>
        writer.writeTo(index, f(b), out)
      }
  }
}

object PBWriter extends PBWriterImplicits {
  def apply[A : PBWriter]: PBWriter[A] = implicitly
}
