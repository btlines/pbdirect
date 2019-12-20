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

  implicit def consWriter[H, T <: HList](
      implicit head: PBFieldWriter[H],
      tail: Lazy[PBProductWriter[T]]): PBProductWriter[(FieldIndex, H) :: T] =
    instance { (indexedValues: (FieldIndex, H) :: T, out: CodedOutputStream) =>
      val headIndex = indexedValues.head._1.values.head
      val headValue = indexedValues.head._2
      head.writeTo(headIndex, headValue, out)
      tail.value.writeTo(indexedValues.tail, out)
    }

}

object PBProductWriter extends PBProductWriterImplicits {
  def apply[A <: HList: PBProductWriter]: PBProductWriter[A] = implicitly
}
