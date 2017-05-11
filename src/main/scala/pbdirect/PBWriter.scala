package pbdirect

import java.io.ByteArrayOutputStream

import cats.Functor
import com.google.protobuf.CodedOutputStream
import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy }

trait PBWriter[A] {
  def writeTo(index: Int, value: A, out: CodedOutputStream): Unit
}

trait LowPriorityPBWriterImplicits {
  implicit object HNilWriter extends PBWriter[HNil] {
    override def writeTo(index: Int, value: HNil, out: CodedOutputStream): Unit = ()
  }
  implicit def consWriter[H, T <: HList](implicit head: PBWriter[H], tail: Lazy[PBWriter[T]]): PBWriter[H :: T] =
    new PBWriter[H :: T] {
      override def writeTo(index: Int, value: H :: T, out: CodedOutputStream): Unit = {
        head.writeTo(index, value.head, out)
        tail.value.writeTo(index + 1, value.tail, out)
      }
    }
  implicit def prodWriter[A, R <: HList](implicit gen: Generic.Aux[A, R], writer: PBWriter[R]): PBWriter[A] =
    new PBWriter[A] {
      override def writeTo(index: Int, value: A, out: CodedOutputStream): Unit = {
        val buffer = new ByteArrayOutputStream()
        val pbOut = CodedOutputStream.newInstance(buffer)
        writer.writeTo(1, gen.to(value), pbOut)
        pbOut.flush()
        out.writeByteArray(index, buffer.toByteArray)
      }
    }

  implicit object CNilWriter extends PBWriter[CNil] {
    override def writeTo(index: Int, value: CNil, out: CodedOutputStream): Unit = ()
  }
  implicit def cconsWriter[H, T <: Coproduct](implicit head: PBWriter[H], tail: PBWriter[T]): PBWriter[H :+: T] =
    new PBWriter[H :+: T] {
      override def writeTo(index: Int, value: H :+: T, out: CodedOutputStream): Unit =
        value match {
          case Inl(h) => head.writeTo(index, h, out)
          case Inr(t) => tail.writeTo(index, t, out)
        }
    }
  implicit def coprodWriter[A, R <: Coproduct](implicit gen: Generic.Aux[A, R], writer: PBWriter[R]): PBWriter[A] =
    new PBWriter[A] {
      override def writeTo(index: Int, value: A, out: CodedOutputStream): Unit =
        writer.writeTo(index, gen.to(value), out)
    }
}

trait PBWriterImplicits extends LowPriorityPBWriterImplicits {
  implicit object BooleanWriter extends PBWriter[Boolean] {
    override def writeTo(index: Int, value: Boolean, out: CodedOutputStream): Unit =
      out.writeBool(index, value)
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
  implicit def functorWriter[F[_], A](implicit functor: Functor[F], writer: PBWriter[A]): PBWriter[F[A]] =
    new PBWriter[F[A]] {
      override def writeTo(index: Int, value: F[A], out: CodedOutputStream): Unit = {
        functor.map(value) { v => writer.writeTo(index, v, out) }
        ()
      }
    }
  implicit def mapWriter[K, V](implicit writer: PBWriter[List[(K, V)]]): PBWriter[Map[K, V]] =
    new PBWriter[Map[K, V]] {
      override def writeTo(index: Int, value: Map[K, V], out: CodedOutputStream) =
        writer.writeTo(index, value.toList, out)
    }
  implicit def enumWriter[E](implicit values: Enum.Values[E], ordering: Ordering[E]): PBWriter[E] =
    new PBWriter[E] {
      override def writeTo(index: Int, value: E, out: CodedOutputStream): Unit =
        out.writeInt32(index, Enum.toInt(value))
    }
}

object PBWriter extends PBWriterImplicits
