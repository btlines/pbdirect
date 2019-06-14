package pbdirect

import java.io.ByteArrayOutputStream

import cats.data.{NonEmptyList => NEL}
import cats.{Contravariant, Functor}
import com.google.protobuf.CodedOutputStream
import shapeless.{:+:, ::, Annotations, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}
import shapeless.ops.hlist.ToList
import scala.collection.GenMap
import pbdirect.LowPriorityPBWriterImplicits.SizeWithoutTag

trait PBWriter[A] {
  def writeTo(index: NEL[Int], value: A, out: CodedOutputStream, sizes: SizeWithoutTag): Unit
  def writtenBytesSize(index: NEL[Int], value: A, sizes: SizeWithoutTag): Int
}

object LowPriorityPBWriterImplicits {
  type SizeWithoutTag = java.util.Map[Any, Int]

  private[pbdirect] def memoizeSizeWithoutTag[A](writtenBytesSizeWithoutTag: (NEL[Int], A, SizeWithoutTag) => Int): (NEL[Int], A, SizeWithoutTag) => Int = {
    (index: NEL[Int], value: A, sizes: SizeWithoutTag) => {
      val sizeWithoutTag = if (sizes.containsKey(value)) {
        sizes.get(value)
      } else {
        val size = writtenBytesSizeWithoutTag(index, value, sizes)
        sizes.put(value, size)
        size
      }
      CodedOutputStream.computeTagSize(index.head) + sizeWithoutTag
    }
  }
}

trait LowPriorityPBWriterImplicits {

  def instance[A](serialize: (NEL[Int], A, CodedOutputStream, SizeWithoutTag) => Unit, computeBytesSize: (NEL[Int], A, SizeWithoutTag) => Int): PBWriter[A] =
    new PBWriter[A] {
      override def writeTo(index: NEL[Int], value: A, out: CodedOutputStream, sizes: SizeWithoutTag): Unit = serialize(index, value, out, sizes)
      override def writtenBytesSize(index: NEL[Int], value: A, sizes: SizeWithoutTag): Int = computeBytesSize(index, value, sizes)
    }
  
  implicit val hnilWriter: PBWriter[HNil] = instance(
  { (_: NEL[Int], _: HNil, _: CodedOutputStream, _: SizeWithoutTag) => () },
  { (_: NEL[Int], _: HNil, _: SizeWithoutTag) => 0 }
  )

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
      val fields = NEL
        .fromList((1 to annotationList.size).toList.zip(annotationList).map {
          case (i, None)           => i
          case (_, Some(Index(i))) => i
        })
        .getOrElse(NEL.one(1))

      writer.value.writeTo(fields, gen.to(value), pbOut)
      pbOut.flush()
      out.writeByteArray(index.head, buffer.toByteArray)
    }
  implicit val cnilWriter: PBWriter[CNil] = instance(
  { (_: NEL[Int], _: CNil, _: CodedOutputStream, _: SizeWithoutTag) => throw new Exception("Can't write CNil") },
  { (_: NEL[Int], _: CNil, _: SizeWithoutTag) => 0 }
  )
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
    implicit h1: PBWriter[H1],
    h2: PBWriter[H2],
    tail: Lazy[PBWriter[T]]
  ): PBWriter[H1 :: H2 :: T] =
    instance { (index: NEL[Int], value: H1 :: H2 :: T, out: CodedOutputStream) =>
      h1.writeTo(index, value.head, out)
      NEL.fromList(index.tail).foreach(h2.writeTo(_, value.tail.head, out))
      NEL.fromList(index.tail.tail).foreach(tail.value.writeTo(_, value.tail.tail, out))
    }

  implicit def cconsWriter2[H1, H2, T <: Coproduct](
    implicit h1: PBWriter[H1],
    h2: PBWriter[H2],
    tail: PBWriter[T]
  ): PBWriter[H1 :+: H2 :+: T] =
    instance { (index: NEL[Int], value: H1 :+: H2 :+: T, out: CodedOutputStream) =>
      value match {
        case Inl(v)      => h1.writeTo(index, v, out)
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
          case Inl(v)                => h1.writeTo(index, v, out)
          case Inr(Inl(v))           => h2.writeTo(index, v, out)
          case Inr(Inr(Inl(v)))      => h3.writeTo(index, v, out)
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
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail)
          .foreach(h7.writeTo(_, value.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail)
          .foreach(h8.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(tail.value.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail, out))
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
          case Inl(v)                                    => h1.writeTo(index, v, out)
          case Inr(Inl(v))                               => h2.writeTo(index, v, out)
          case Inr(Inr(Inl(v)))                          => h3.writeTo(index, v, out)
          case Inr(Inr(Inr(Inl(v))))                     => h4.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inl(v)))))                => h5.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inl(v))))))           => h6.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))      => h7.writeTo(index, v, out)
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
  ): PBWriter[
    H1 :: H2 :: H3 :: H4 :: H5 :: H6 :: H7 :: H8 :: H9 :: H10 :: H11 :: H12 :: H13 :: H14 :: H15 :: H16 :: T
  ] =
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
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail)
          .foreach(h7.writeTo(_, value.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail)
          .foreach(h8.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(h9.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(h10.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(h11.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(h12.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(h13.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(h14.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out))
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(
            h15.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out)
          )
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(
            h16.writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head, out)
          )
        NEL
          .fromList(index.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail)
          .foreach(
            tail.value
              .writeTo(_, value.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail, out)
          )
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
  ): PBWriter[
    H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: H9 :+: H10 :+: H11 :+: H12 :+: H13 :+: H14 :+: H15 :+: H16 :+: T
  ] =
    instance {
      (
        index: NEL[Int],
        value: H1 :+: H2 :+: H3 :+: H4 :+: H5 :+: H6 :+: H7 :+: H8 :+: H9 :+: H10 :+: H11 :+: H12 :+: H13 :+: H14 :+: H15 :+: H16 :+: T,
        out: CodedOutputStream
      ) =>
        value match {
          case Inl(v)                                                                  => h1.writeTo(index, v, out)
          case Inr(Inl(v))                                                             => h2.writeTo(index, v, out)
          case Inr(Inr(Inl(v)))                                                        => h3.writeTo(index, v, out)
          case Inr(Inr(Inr(Inl(v))))                                                   => h4.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inl(v)))))                                              => h5.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inl(v))))))                                         => h6.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))                                    => h7.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))))                               => h8.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))                          => h9.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))))))                     => h10.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))))                => h11.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))))))))           => h12.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))))))      => h13.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))))))) => h14.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v))))))))))))))) =>
            h15.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(v)))))))))))))))) =>
            h16.writeTo(index, v, out)
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr((v))))))))))))))))) =>
            tail.writeTo(index, v, out)
        }
    }
}

trait PBWriterImplicits extends PBConsWriter16 {
  implicit object BooleanWriter extends PBWriter[Boolean] {
    override def writeTo(index: NEL[Int], value: Boolean, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeBool(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: Boolean, sizes: SizeWithoutTag): Int =
      CodedOutputStream.computeBoolSize(index.head, value)
  }
  implicit object ByteWriter extends PBWriter[Byte] {
    override def writeTo(index: NEL[Int], value: Byte, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeInt32(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: Byte, sizes: SizeWithoutTag): Int =
      CodedOutputStream.computeInt32Size(index.head, value)
  }
  implicit object ShortWriter extends PBWriter[Short] {
    override def writeTo(index: NEL[Int], value: Short, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeInt32(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: Short, sizes: SizeWithoutTag): Int =
      CodedOutputStream.computeInt32Size(index.head, value)
  }
  implicit object IntWriter extends PBWriter[Int] {
    override def writeTo(index: NEL[Int], value: Int, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeInt32(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: Int, sizes: SizeWithoutTag): Int =
      CodedOutputStream.computeInt32Size(index.head, value)
  }
  implicit object LongWriter extends PBWriter[Long] {
    override def writeTo(index: NEL[Int], value: Long, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeInt64(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: Long, sizes: SizeWithoutTag): Int =
      CodedOutputStream.computeInt64Size(index.head, value)
  }
  implicit object FloatWriter extends PBWriter[Float] {
    override def writeTo(index: NEL[Int], value: Float, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeFloat(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: Float, sizes: SizeWithoutTag): Int =
      CodedOutputStream.computeFloatSize(index.head, value)
  }
  implicit object DoubleWriter extends PBWriter[Double] {
    override def writeTo(index: NEL[Int], value: Double, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeDouble(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: Double, sizes: SizeWithoutTag): Int =
      CodedOutputStream.computeDoubleSize(index.head, value)
  }
  implicit object StringWriter extends PBWriter[String] {
    override def writeTo(index: NEL[Int], value: String, out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeString(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: String, sizes: SizeWithoutTag): Int =
    //NOTE: This is potentially an expensive operation if the string contains
    // unusual characters.
      CodedOutputStream.computeStringSize(index.head, value)
  }
  implicit object BytesWriter extends PBWriter[Array[Byte]] {
    override def writeTo(index: NEL[Int], value: Array[Byte], out: CodedOutputStream, sizes: SizeWithoutTag): Unit =
      out.writeByteArray(index.head, value)
    override def writtenBytesSize(index: NEL[Int], value: Array[Byte], sizes: SizeWithoutTag): Int =
      CodedOutputStream.computeByteArraySize(index.head, value)
  }
  implicit def functorWriter[F[_], A](implicit functor: Functor[F], writer: PBWriter[A]): PBWriter[F[A]] =
    instance(
    { (index: NEL[Int], value: F[A], out: CodedOutputStream, sizes: SizeWithoutTag) =>
      functor.map(value) { v => writer.writeTo(index, v, out, sizes) }
      ()
    },
    { (index: NEL[Int], value: F[A], sizes: SizeWithoutTag) =>
      var total = 0
      functor.map(value) { v => total += writer.writtenBytesSize(index, v, sizes) }
      total
    }
    )
  implicit def mapWriter[K, V](implicit writer: PBWriter[List[(K, V)]]): PBWriter[Map[K, V]] =
    instance(
    { (index: NEL[Int], value: Map[K, V], out: CodedOutputStream, sizes: SizeWithoutTag) =>
      writer.writeTo(index, value.toList, out, sizes)
    },
    { (index: NEL[Int], value: Map[K, V], sizes: SizeWithoutTag) =>
      writer.writtenBytesSize(index, value.toList, sizes)
    }
    )
  implicit def collectionMapWriter[K, V](implicit writer: PBWriter[List[(K, V)]]): PBWriter[collection.Map[K, V]] =
    instance(
    { (index: NEL[Int], value: collection.Map[K, V], out: CodedOutputStream, sizes: SizeWithoutTag) =>
      writer.writeTo(index, value.toList, out, sizes)
    },
    { (index: NEL[Int], value: collection.Map[K, V], sizes: SizeWithoutTag) =>
      writer.writtenBytesSize(index, value.toList, sizes)
    }
    )
  implicit def seqWriter[A](implicit writer: PBWriter[List[A]]): PBWriter[Seq[A]] =
    instance(
    { (index: NEL[Int], value: Seq[A], out: CodedOutputStream, sizes: SizeWithoutTag) =>
      writer.writeTo(index, value.toList, out, sizes)
    },
    { (index: NEL[Int], value: Seq[A], sizes: SizeWithoutTag) =>
      writer.writtenBytesSize(index, value.toList, sizes)
    }
    )
  implicit def enumWriter[E](implicit values: Enum.Values[E], ordering: Ordering[E]): PBWriter[E] =
    instance(
    { (index: NEL[Int], value: E, out: CodedOutputStream, sizes: SizeWithoutTag) =>
      out.writeInt32(index.head, Enum.toInt(value))
    },
    { (index: NEL[Int], value: E, sizes: SizeWithoutTag) =>
      CodedOutputStream.computeInt32Size(index.head, Enum.toInt(value))
    }
    )
  implicit def enumerationWriter[E <: Enumeration#Value]: PBWriter[E] =
    instance(
    { (index: NEL[Int], value: E, out: CodedOutputStream, sizes: SizeWithoutTag) =>
      out.writeInt32(index.head, value.id)
    },
    { (index: NEL[Int], value: E, sizes: SizeWithoutTag) =>
      CodedOutputStream.computeInt32Size(index.head, value.id)
    }
    )

  implicit object ContravariantWriter extends Contravariant[PBWriter] {
    override def contramap[A, B](writer: PBWriter[A])(f: B => A): PBWriter[B] =
      instance(
      { (index: NEL[Int], b: B, out: CodedOutputStream, sizes: SizeWithoutTag) =>
        writer.writeTo(index, f(b), out, sizes)
      },
      { (index: NEL[Int], b: B, sizes: SizeWithoutTag) =>
        writer.writtenBytesSize(index, f(b), sizes)
      }
      )
  }
}

object PBWriter extends PBWriterImplicits {
  def apply[A: PBWriter]: PBWriter[A] = implicitly
}






//
//trait LowPriorityPBWriterImplicits {
//  implicit def consWriter[H, T <: HList](implicit head: PBWriter[H], tail: Lazy[PBWriter[T]]): PBWriter[H :: T] =
//    instance(
//    { (index: Int, value: H :: T, out: CodedOutputStream, sizes: SizeWithoutTag) =>
//      head.writeTo(index, value.head, out, sizes)
//      tail.value.writeTo(index + 1, value.tail, out, sizes)
//    },
//    { (index: Int, value: H :: T, sizes: SizeWithoutTag) =>
//      head.writtenBytesSize(index, value.head, sizes) +
//      tail.value.writtenBytesSize(index + 1, value.tail, sizes)
//    }
//    )
//  implicit def prodWriter[A, R <: HList](implicit gen: Generic.Aux[A, R], writer: Lazy[PBWriter[R]]): PBWriter[A] =
//    instance(
//    { (index: Int, value: A, out: CodedOutputStream, sizes: SizeWithoutTag) =>
//      val valueAsHList = gen.to(value)
//      val size = writer.value.writtenBytesSize(1, valueAsHList, sizes)
//      out.writeTag(index, WireFormat.WIRETYPE_LENGTH_DELIMITED)
//      out.writeUInt32NoTag(size)
//      writer.value.writeTo(1, valueAsHList, out, sizes)
//    },
//    LowPriorityPBWriterImplicits.memoizeSizeWithoutTag { (index: Int, value: A, sizes: SizeWithoutTag) =>
//      val bytesSize = writer.value.writtenBytesSize(1, gen.to(value), sizes)
//      CodedOutputStream.computeUInt32SizeNoTag(bytesSize) + bytesSize
//    }
//    )
//
//  implicit def cconsWriter[H, T <: Coproduct](implicit head: PBWriter[H], tail: PBWriter[T]): PBWriter[H :+: T] =
//    instance(
//    { (index: Int, value: H :+: T, out: CodedOutputStream, sizes: SizeWithoutTag) =>
//      value match {
//        case Inl(h) => head.writeTo(index, h, out, sizes)
//        case Inr(t) => tail.writeTo(index, t, out, sizes)
//      }
//    },
//    { (index: Int, value: H :+: T, sizes: SizeWithoutTag) =>
//      value match {
//        case Inl(h) => head.writtenBytesSize(index, h, sizes)
//        case Inr(t) => tail.writtenBytesSize(index, t, sizes)
//      }
//    }
//    )
//  implicit def coprodWriter[A, R <: Coproduct](implicit gen: Generic.Aux[A, R], writer: PBWriter[R]): PBWriter[A] =
//    instance(
//    { (index: Int, value: A, out: CodedOutputStream, sizes: SizeWithoutTag) =>
//      writer.writeTo(index, gen.to(value), out, sizes)
//    },
//    { (index: Int, value: A, sizes: SizeWithoutTag) =>
//      writer.writtenBytesSize(index, gen.to(value), sizes)
//    }
//    )
//}
