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

  object zipWithModifiers extends Poly2 {
    implicit def annotatedCase[A] = at[(FieldIndex, A), Some[pbUnpacked]] {
      case ((fieldIndex, value), Some(annotation)) =>
        (fieldIndex, value, FieldModifiers(unpacked = true))
    }
    implicit def unannotatedCase[A] = at[(FieldIndex, A), None.type] {
      case ((fieldIndex, value), None) =>
        (fieldIndex, value, FieldModifiers(unpacked = false))
    }
  }

  implicit def prodWriter[
      A,
      R <: HList,
      IA <: HList,
      UA <: HList,
      ZWI <: HList,
      ZWFI <: HList,
      ZWM <: HList
  ](
      implicit gen: Generic.Aux[A, R],
      indexAnns: Annotations.Aux[pbIndex, A, IA],
      unpackedAnns: Annotations.Aux[pbUnpacked, A, UA],
      zwi: ZipWithIndex.Aux[R, ZWI],
      zwfi: ZipWith.Aux[IA, ZWI, zipWithFieldIndex.type, ZWFI],
      zwm: ZipWith.Aux[ZWFI, UA, zipWithModifiers.type, ZWM],
      writer: Lazy[PBProductWriter[ZWM]]
  ): PBMessageWriter[A] =
    instance { (value: A, out: CodedOutputStream) =>
      val fields                     = gen.to(value)
      val fieldsWithIndices          = fields.zipWithIndex
      val indexedFields              = indexAnns.apply.zipWith(fieldsWithIndices)(zipWithFieldIndex)
      val indexedFieldsWithModifiers = indexedFields.zipWith(unpackedAnns.apply)(zipWithModifiers)
      writer.value.writeTo(indexedFieldsWithModifiers, out)
    }

}

object PBMessageWriter extends PBMessageWriterImplicits {
  def apply[A: PBMessageWriter]: PBMessageWriter[A] = implicitly
}
