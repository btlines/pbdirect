package pbdirect

import cats.Functor
import com.google.protobuf.CodedOutputStream

trait PBFieldWriter[A] {
  def writeTo(index: Int, value: A, out: CodedOutputStream): Unit
}

trait LowPriorityPBFieldWriterImplicits {
  def instance[A](f: (Int, A, CodedOutputStream) => Unit): PBFieldWriter[A] =
    new PBFieldWriter[A] {
      override def writeTo(index: Int, value: A, out: CodedOutputStream): Unit =
        f(index, value, out)
    }

  implicit def functorWriter[F[_], A](
      implicit functor: Functor[F],
      writer: PBFieldWriter[A]): PBFieldWriter[F[A]] =
    instance { (index: Int, value: F[A], out: CodedOutputStream) =>
      functor.map(value) { v =>
        writer.writeTo(index, v, out)
      }
      ()
    }
}

trait PBFieldWriterImplicits extends LowPriorityPBFieldWriterImplicits {

  implicit def scalarWriter[A](implicit writer: PBScalarValueWriter[A]): PBFieldWriter[A] =
    instance { (index: Int, value: A, out: CodedOutputStream) =>
      out.writeTag(index, writer.wireType)
      writer.writeWithoutTag(value, out)
    }

  implicit def optionWriter[A](implicit writer: PBFieldWriter[A]): PBFieldWriter[Option[A]] =
    instance { (index: Int, option: Option[A], out: CodedOutputStream) =>
      option.foreach(v => writer.writeTo(index, v, out))
    }

  implicit def listWriter[A](implicit writer: PBFieldWriter[A]): PBFieldWriter[List[A]] =
    instance { (index: Int, list: List[A], out: CodedOutputStream) =>
      list.foreach(v => writer.writeTo(index, v, out))
    }

  implicit def mapWriter[K, V](
      implicit writer: PBFieldWriter[List[(K, V)]]): PBFieldWriter[Map[K, V]] =
    instance { (index: Int, value: Map[K, V], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }

  implicit def collectionMapWriter[K, V](
      implicit writer: PBFieldWriter[List[(K, V)]]): PBFieldWriter[collection.Map[K, V]] =
    instance { (index: Int, value: collection.Map[K, V], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }

  implicit def seqWriter[A](implicit writer: PBFieldWriter[List[A]]): PBFieldWriter[Seq[A]] =
    instance { (index: Int, value: Seq[A], out: CodedOutputStream) =>
      writer.writeTo(index, value.toList, out)
    }

}

object PBFieldWriter extends PBFieldWriterImplicits {
  def apply[A: PBFieldWriter]: PBFieldWriter[A] = implicitly
}
