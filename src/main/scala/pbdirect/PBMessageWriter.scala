package pbdirect

import com.google.protobuf.CodedOutputStream
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.nat._

trait PBMessageWriter[A] {
  def writeTo(value: A, out: CodedOutputStream): Unit
}

trait PBMessageWriterImplicits {

  def instance[A](f: (A, CodedOutputStream) => Unit): PBMessageWriter[A] =
    new PBMessageWriter[A] {
      override def writeTo(value: A, out: CodedOutputStream): Unit =
        f(value, out)
    }

  object zipWithFieldIndex extends Poly2 {
    implicit def annotatedCase[T, N <: Nat] = at[Some[pbIndex], (T, N)] {
      case (Some(annotation), (value, n)) =>
        (FieldIndex(annotation.first :: annotation.more.toList), value)
    }
    implicit def unannotatedCase[T, N <: Nat](implicit toInt: ToInt[N]) = at[None.type, (T, N)] {
      case (None, (value, n)) =>
        (FieldIndex(List(toInt() + 1)), value)
    }
  }

  implicit def prodWriter[A, R <: HList, Anns <: HList, ZWI <: HList, ZWFI <: HList](
      implicit gen: Generic.Aux[A, R],
      annotations: Annotations.Aux[pbIndex, A, Anns],
      zwi: ZipWithIndex.Aux[R, ZWI],
      zw: ZipWith.Aux[Anns, ZWI, zipWithFieldIndex.type, ZWFI],
      writer: Lazy[PBProductWriter[ZWFI]]): PBMessageWriter[A] =
    instance { (value: A, out: CodedOutputStream) =>
      val fields            = gen.to(value)
      val fieldsWithIndices = fields.zipWithIndex
      val indexedFields     = annotations.apply.zipWith(fieldsWithIndices)(zipWithFieldIndex)
      writer.value.writeTo(indexedFields, out)
    }

  implicit val cnilWriter: PBMessageWriter[CNil] = instance { (_: CNil, _: CodedOutputStream) =>
    throw new Exception("Can't write CNil")
  }
  implicit def cconsWriter[H, T <: Coproduct](
      implicit head: PBMessageWriter[H],
      tail: PBMessageWriter[T]): PBMessageWriter[H :+: T] =
    instance { (value: H :+: T, out: CodedOutputStream) =>
      value match {
        case Inl(h) => head.writeTo(h, out)
        case Inr(t) => tail.writeTo(t, out)
      }
    }
  implicit def coprodWriter[A, R <: Coproduct](
      implicit gen: Generic.Aux[A, R],
      writer: PBMessageWriter[R]): PBMessageWriter[A] =
    instance { (value: A, out: CodedOutputStream) =>
      writer.writeTo(gen.to(value), out)
    }

}

object PBMessageWriter extends PBMessageWriterImplicits {
  def apply[A: PBMessageWriter]: PBMessageWriter[A] = implicitly
}
