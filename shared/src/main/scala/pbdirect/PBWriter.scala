package pbdirect

import java.io.ByteArrayOutputStream

import cats.{ Contravariant, Functor }
import com.google.protobuf.CodedOutputStream
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

trait PBWriter[A] {
  def writeTo(index: Int, value: A, out: CodedOutputStream): Unit
}

trait LowPriorityPBWriterImplicits {
  def instance[A](f: (Int, A, CodedOutputStream) => Unit): PBWriter[A] =
    new PBWriter[A] {
      override def writeTo(index: Int, value: A, out: CodedOutputStream): Unit = f(index, value, out)
    }
  implicit val hnilWriter: PBWriter[HNil] = instance {
    (_: Int, _: HNil, _: CodedOutputStream) => ()
  }
  implicit def consWriter[H, T <: HList](implicit head: PBWriter[H], tail: Lazy[PBWriter[T]]): PBWriter[H :: T] =
    instance { (index: Int, value: H :: T, out: CodedOutputStream) =>
      head.writeTo(index, value.head, out)
      tail.value.writeTo(index + 1, value.tail, out)
    }
  implicit def prodWriter[A, R <: HList](implicit gen: Generic.Aux[A, R], writer: Lazy[PBWriter[R]]): PBWriter[A] =
    instance { (index: Int, value: A, out: CodedOutputStream) =>
      val buffer = new ByteArrayOutputStream()
      val pbOut = CodedOutputStream.newInstance(buffer)
      writer.value.writeTo(1, gen.to(value), pbOut)
      pbOut.flush()
      out.writeByteArray(index, buffer.toByteArray)
    }

  implicit val cnilWriter: PBWriter[CNil] = instance {
    (_: Int, _: CNil, _: CodedOutputStream) => throw new Exception("Can't write CNil")
  }
  implicit def cconsWriter[H, T <: Coproduct](implicit head: PBWriter[H], tail: PBWriter[T]): PBWriter[H :+: T] =
    instance { (index: Int, value: H :+: T, out: CodedOutputStream) =>
      value match {
        case Inl(h) => head.writeTo(index, h, out)
        case Inr(t) => tail.writeTo(index, t, out)
      }
    }
  implicit def coprodWriter[A, R <: Coproduct](implicit gen: Generic.Aux[A, R], writer: PBWriter[R]): PBWriter[A] =
    instance { (index: Int, value: A, out: CodedOutputStream) =>
      writer.writeTo(index, gen.to(value), out)
    }
}

trait PBWriterImplicits extends LowPriorityPBWriterImplicits {
  implicit object BooleanWriter extends PBWriter[Boolean] {
    override def writeTo(index: Int, value: Boolean, out: CodedOutputStream): Unit =
      out.writeBool(index, value)
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
    override def writeTo(index: Int, value: Int, out: CodedOutputStream): Unit =
      out.writeInt32(index, value)
  }
  implicit object LongWriter extends PBWriter[Long] {
    override def writeTo(index: Int, value: Long, out: CodedOutputStream): Unit =
      out.writeInt64(index, value)
  }
  implicit object FloatWriter extends PBWriter[Float] {
    override def writeTo(index: Int, value: Float, out: CodedOutputStream): Unit =
      out.writeFloat(index, value)
  }
  implicit object DoubleWriter extends PBWriter[Double] {
    override def writeTo(index: Int, value: Double, out: CodedOutputStream): Unit =
      out.writeDouble(index, value)
  }
  implicit object StringWriter extends PBWriter[String] {
    override def writeTo(index: Int, value: String, out: CodedOutputStream): Unit =
      out.writeString(index, value)
  }
  implicit object BytesWriter extends PBWriter[Array[Byte]] {
    override def writeTo(index: Int, value: Array[Byte], out: CodedOutputStream): Unit =
      out.writeByteArray(index, value)
  }
  implicit def functorWriter[F[_], A](implicit functor: Functor[F], writer: PBWriter[A]): PBWriter[F[A]] =
    instance { (index: Int, value: F[A], out: CodedOutputStream) =>
      functor.map(value) { v => writer.writeTo(index, v, out) }
      ()
    }
  implicit def mapWriter[K, V](implicit writer: PBWriter[List[(K, V)]]): PBWriter[Map[K, V]] =
    instance { (index: Int, value: Map[K, V], out: CodedOutputStream) =>
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
    instance { (index: Int, value: E, out: CodedOutputStream) =>
      out.writeInt32(index, Enum.toInt(value))
    }
  implicit def enumerationWriter[E <: Enumeration#Value]: PBWriter[E] =
    instance { (index: Int, value: E, out: CodedOutputStream) =>
      out.writeInt32(index, value.id)
    }

  implicit object ContravariantWriter extends Contravariant[PBWriter] {
    override def contramap[A, B](writer: PBWriter[A])(f: B => A) =
      instance { (index: Int, b: B, out: CodedOutputStream) =>
        writer.writeTo(index, f(b), out)
      }
  }
}

object PBWriter extends PBWriterImplicits {
  def apply[A : PBWriter]: PBWriter[A] = implicitly
}
