package pbdirect

import com.google.protobuf.CodedOutputStream
import shapeless._

trait PBProductWriter[A <: HList] {
  def writeTo(indexedValues: A, out: CodedOutputStream): Unit
}

trait PBProductWriterImplicits {
  def instance[A <: HList](f: (A, CodedOutputStream) => Unit): PBProductWriter[A] =
    new PBProductWriter[A] {
      override def writeTo(indexedValues: A, out: CodedOutputStream): Unit =
        f(indexedValues, out)
    }

  implicit val hnilWriter: PBProductWriter[HNil] = instance { (_: HNil, _: CodedOutputStream) =>
    ()
  }

  implicit def hconsWriter[H, T <: HList](implicit
      head: PBFieldWriter[H],
      tail: Lazy[PBProductWriter[T]]
  ): PBProductWriter[(FieldIndex, H, FieldModifiers) :: T] =
    instance { (indexedValues: (FieldIndex, H, FieldModifiers) :: T, out: CodedOutputStream) =>
      val headIndex     = indexedValues.head._1.values.head
      val headValue     = indexedValues.head._2
      val headModifiers = indexedValues.head._3
      val flags         = PBFieldWriter.Flags(skipDefaultValue = true, unpacked = headModifiers.unpacked)
      head.writeTo(headIndex, headValue, out, flags)
      tail.value.writeTo(indexedValues.tail, out)
    }

  implicit def hconsCoproductOneofWriter[H <: Coproduct, T <: HList](implicit
      head: PBOneofFieldWriter[H],
      tail: Lazy[PBProductWriter[T]]
  ): PBProductWriter[(FieldIndex, Option[H], FieldModifiers) :: T] =
    instance {
      (indexedValues: (FieldIndex, Option[H], FieldModifiers) :: T, out: CodedOutputStream) =>
        indexedValues.head match {
          case (headFieldIndex, Some(headValue), headModifiers) =>
            val flags =
              PBFieldWriter.Flags(skipDefaultValue = true, unpacked = headModifiers.unpacked)
            head.writeTo(headFieldIndex.values, headValue, out, flags)
          case _ => // skip writing the field
        }
        tail.value.writeTo(indexedValues.tail, out)
    }

  // write an Either[A, B] by treating it as a Coproduct (Left[A, B] :+: Right[A, B] :+: CNil)
  implicit def hconsEitherOneofWriter[A, B, H <: Coproduct, T <: HList](implicit
      gen: Generic.Aux[Either[A, B], H],
      productWriter: PBProductWriter[(FieldIndex, Option[H], FieldModifiers) :: T]
  ): PBProductWriter[(FieldIndex, Option[Either[A, B]], FieldModifiers) :: T] =
    instance {
      (
          indexedValues: (FieldIndex, Option[Either[A, B]], FieldModifiers) :: T,
          out: CodedOutputStream
      ) =>
        val headEitherAsCoproduct: Option[H] = indexedValues.head._2.map(gen.to)
        val head                             = indexedValues.head.copy(_2 = headEitherAsCoproduct)
        productWriter.writeTo(head :: indexedValues.tail, out)
    }

}

object PBProductWriter extends PBProductWriterImplicits {
  def apply[A <: HList: PBProductWriter]: PBProductWriter[A] = implicitly
}
