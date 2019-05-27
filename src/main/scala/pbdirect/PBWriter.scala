package pbdirect

import cats.{Contravariant, Functor}
import com.google.protobuf.{CodedOutputStream, WireFormat}
import pbdirect.LowPriorityPBWriterImplicits.SizeMap
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

trait PBWriter[A] {
  def writeTo(index: Int, value: A, out: CodedOutputStream, sizes: SizeMap): Unit
  def writtenBytesSize(index: Int, value: A, sizes: SizeMap): Int
}

object LowPriorityPBWriterImplicits {
  type SizeMap = java.util.Map[Any, Int]
  
  private[pbdirect] def memoize[A](writtenBytesSize: (Int, A, SizeMap) => Int): (Int, A, SizeMap) => Int = {
    (index: Int, value: A, sizes: SizeMap) => {
      if (sizes.containsKey(value)) {
        sizes.get(value)
      } else {
        val size = writtenBytesSize(index, value, sizes)
        sizes.put(value, size)
        size
      }
    }
  }
}

trait LowPriorityPBWriterImplicits {
  def instance[A](serialize: (Int, A, CodedOutputStream, SizeMap) => Unit, computeBytesSize: (Int, A, SizeMap) => Int): PBWriter[A] =
    new PBWriter[A] {
      override def writeTo(index: Int, value: A, out: CodedOutputStream, sizes: SizeMap): Unit = serialize(index, value, out, sizes)
      override def writtenBytesSize(index: Int, value: A, sizes: SizeMap): Int = computeBytesSize(index, value, sizes)
    }
  
  implicit val hnilWriter: PBWriter[HNil] = instance(
    { (_: Int, _: HNil, _: CodedOutputStream, _: SizeMap) => () },
    { (_: Int, _: HNil, _: SizeMap) => 0 }
  )
  implicit def consWriter[H, T <: HList](implicit head: PBWriter[H], tail: Lazy[PBWriter[T]]): PBWriter[H :: T] =
    instance(
      { (index: Int, value: H :: T, out: CodedOutputStream, sizes: SizeMap) =>
        head.writeTo(index, value.head, out, sizes)
        tail.value.writeTo(index + 1, value.tail, out, sizes)
      },
      { (index: Int, value: H :: T, sizes: SizeMap) =>
        head.writtenBytesSize(index, value.head, sizes) +
        tail.value.writtenBytesSize(index + 1, value.tail, sizes)
      }
    )
  implicit def prodWriter[A, R <: HList](implicit gen: Generic.Aux[A, R], writer: Lazy[PBWriter[R]]): PBWriter[A] =
    instance(
      { (index: Int, value: A, out: CodedOutputStream, sizes: SizeMap) =>
        val valueAsHList = gen.to(value)
        val size = writer.value.writtenBytesSize(1, valueAsHList, sizes)
        out.writeTag(index, WireFormat.WIRETYPE_LENGTH_DELIMITED)
        out.writeUInt32NoTag(size)
        writer.value.writeTo(1, valueAsHList, out, sizes)
      },
      LowPriorityPBWriterImplicits.memoize { (index: Int, value: A, sizes: SizeMap) =>
        val bytesSize = writer.value.writtenBytesSize(1, gen.to(value), sizes)
        CodedOutputStream.computeTagSize(index) + CodedOutputStream.computeUInt32SizeNoTag(bytesSize) + bytesSize
      }
    )

  implicit val cnilWriter: PBWriter[CNil] = instance(
    { (_: Int, _: CNil, _: CodedOutputStream, _: SizeMap) => throw new Exception("Can't write CNil") },
    { (_: Int, _: CNil, _: SizeMap) => 0 }
  )
  implicit def cconsWriter[H, T <: Coproduct](implicit head: PBWriter[H], tail: PBWriter[T]): PBWriter[H :+: T] =
    instance(
      { (index: Int, value: H :+: T, out: CodedOutputStream, sizes: SizeMap) =>
        value match {
          case Inl(h) => head.writeTo(index, h, out, sizes)
          case Inr(t) => tail.writeTo(index, t, out, sizes)
        }
      },
      { (index: Int, value: H :+: T, sizes: SizeMap) =>
        value match {
          case Inl(h) => head.writtenBytesSize(index, h, sizes)
          case Inr(t) => tail.writtenBytesSize(index, t, sizes)
        }
      }
    )
  implicit def coprodWriter[A, R <: Coproduct](implicit gen: Generic.Aux[A, R], writer: PBWriter[R]): PBWriter[A] =
    instance(
      { (index: Int, value: A, out: CodedOutputStream, sizes: SizeMap) =>
        writer.writeTo(index, gen.to(value), out, sizes)
      },
      { (index: Int, value: A, sizes: SizeMap) =>
        writer.writtenBytesSize(index, gen.to(value), sizes)
      }
    )
}

trait PBWriterImplicits extends LowPriorityPBWriterImplicits {
  implicit object BooleanWriter extends PBWriter[Boolean] {
    override def writeTo(index: Int, value: Boolean, out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeBool(index, value)
    override def writtenBytesSize(index: Int, value: Boolean, sizes: SizeMap): Int =
      CodedOutputStream.computeBoolSize(index, value)
  }
  implicit object ByteWriter extends PBWriter[Byte] {
    override def writeTo(index: Int, value: Byte, out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeInt32(index, value)
    override def writtenBytesSize(index: Int, value: Byte, sizes: SizeMap): Int =
      CodedOutputStream.computeInt32Size(index, value)
  }
  implicit object ShortWriter extends PBWriter[Short] {
    override def writeTo(index: Int, value: Short, out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeInt32(index, value)
    override def writtenBytesSize(index: Int, value: Short, sizes: SizeMap): Int =
      CodedOutputStream.computeInt32Size(index, value)
  }
  implicit object IntWriter extends PBWriter[Int] {
    override def writeTo(index: Int, value: Int, out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeInt32(index, value)
    override def writtenBytesSize(index: Int, value: Int, sizes: SizeMap): Int =
      CodedOutputStream.computeInt32Size(index, value)
  }
  implicit object LongWriter extends PBWriter[Long] {
    override def writeTo(index: Int, value: Long, out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeInt64(index, value)
    override def writtenBytesSize(index: Int, value: Long, sizes: SizeMap): Int =
      CodedOutputStream.computeInt64Size(index, value)
  }
  implicit object FloatWriter extends PBWriter[Float] {
    override def writeTo(index: Int, value: Float, out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeFloat(index, value)
    override def writtenBytesSize(index: Int, value: Float, sizes: SizeMap): Int =
      CodedOutputStream.computeFloatSize(index, value)
  }
  implicit object DoubleWriter extends PBWriter[Double] {
    override def writeTo(index: Int, value: Double, out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeDouble(index, value)
    override def writtenBytesSize(index: Int, value: Double, sizes: SizeMap): Int =
      CodedOutputStream.computeDoubleSize(index, value)
  }
  implicit object StringWriter extends PBWriter[String] {
    override def writeTo(index: Int, value: String, out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeString(index, value)
    override def writtenBytesSize(index: Int, value: String, sizes: SizeMap): Int =
      //NOTE: This is potentially an expensive operation if the string contains
      // unusual characters.
      CodedOutputStream.computeStringSize(index, value)
  }
  implicit object BytesWriter extends PBWriter[Array[Byte]] {
    override def writeTo(index: Int, value: Array[Byte], out: CodedOutputStream, sizes: SizeMap): Unit =
      out.writeByteArray(index, value)
    override def writtenBytesSize(index: Int, value: Array[Byte], sizes: SizeMap): Int =
      CodedOutputStream.computeByteArraySize(index, value)
  }
  implicit def functorWriter[F[_], A](implicit functor: Functor[F], writer: PBWriter[A]): PBWriter[F[A]] =
    instance(
      { (index: Int, value: F[A], out: CodedOutputStream, sizes: SizeMap) =>
        functor.map(value) { v => writer.writeTo(index, v, out, sizes) }
        ()
      },
      { (index: Int, value: F[A], sizes: SizeMap) =>
        var total = 0
        functor.map(value) { v => total += writer.writtenBytesSize(index, v, sizes) }
        total
      }
    )
  implicit def mapWriter[K, V](implicit writer: PBWriter[List[(K, V)]]): PBWriter[Map[K, V]] =
    instance(
      { (index: Int, value: Map[K, V], out: CodedOutputStream, sizes: SizeMap) =>
        writer.writeTo(index, value.toList, out, sizes)
      },
      { (index: Int, value: Map[K, V], sizes: SizeMap) =>
        writer.writtenBytesSize(index, value.toList, sizes)
      }
    )
  implicit def collectionMapWriter[K, V](implicit writer: PBWriter[List[(K, V)]]): PBWriter[collection.Map[K, V]] =
    instance(
      { (index: Int, value: collection.Map[K, V], out: CodedOutputStream, sizes: SizeMap) =>
        writer.writeTo(index, value.toList, out, sizes)
      },
      { (index: Int, value: collection.Map[K, V], sizes: SizeMap) =>
        writer.writtenBytesSize(index, value.toList, sizes)
      }
    )
  implicit def seqWriter[A](implicit writer: PBWriter[List[A]]): PBWriter[Seq[A]] =
    instance(
      { (index: Int, value: Seq[A], out: CodedOutputStream, sizes: SizeMap) =>
        writer.writeTo(index, value.toList, out, sizes)
      },
      { (index: Int, value: Seq[A], sizes: SizeMap) =>
        writer.writtenBytesSize(index, value.toList, sizes)
      }
    )
  implicit def enumWriter[E](implicit values: Enum.Values[E], ordering: Ordering[E]): PBWriter[E] =
    instance(
      { (index: Int, value: E, out: CodedOutputStream, sizes: SizeMap) =>
        out.writeInt32(index, Enum.toInt(value))
      },
      { (index: Int, value: E, sizes: SizeMap) =>
        CodedOutputStream.computeInt32Size(index, Enum.toInt(value))
      }
    )
  implicit def enumerationWriter[E <: Enumeration#Value]: PBWriter[E] =
    instance(
      { (index: Int, value: E, out: CodedOutputStream, sizes: SizeMap) =>
        out.writeInt32(index, value.id)
      },
      { (index: Int, value: E, sizes: SizeMap) =>
        CodedOutputStream.computeInt32Size(index, value.id)
      }
    )
  
  implicit object ContravariantWriter extends Contravariant[PBWriter] {
    override def contramap[A, B](writer: PBWriter[A])(f: B => A): PBWriter[B] =
      instance(
        { (index: Int, b: B, out: CodedOutputStream, sizes: SizeMap) =>
          writer.writeTo(index, f(b), out, sizes)
        },
        { (index: Int, b: B, sizes: SizeMap) =>
          writer.writtenBytesSize(index, f(b), sizes)
        }
      )
  }
}

object PBWriter extends PBWriterImplicits {
  def apply[A : PBWriter]: PBWriter[A] = implicitly
}
