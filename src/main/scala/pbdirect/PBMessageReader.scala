package pbdirect

import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.nat._

trait PBMessageReader[A] {
  def read(bytes: Array[Byte]): A
}

trait PBMessageReaderImplicits {

  def instance[A](f: Array[Byte] => A): PBMessageReader[A] =
    new PBMessageReader[A] {
      override def read(bytes: Array[Byte]): A = f(bytes)
    }

  object collectFieldIndices extends Poly1 {
    implicit def annotatedCase[N <: Nat] = at[(Some[pbIndex], N)] {
      case (Some(annotation), _) => FieldIndex(annotation.first :: annotation.more.toList)
    }
    implicit def unannotatedCase[N <: Nat](implicit toInt: ToInt[N]) = at[(None.type, N)] {
      case (None, n) => FieldIndex(List(toInt() + 1))
    }
  }

  implicit def prodReader[A, R <: HList, Anns <: HList, ZWI <: HList, I <: HList](
      implicit
      gen: Generic.Aux[A, R],
      annotations: Annotations.Aux[pbIndex, A, Anns],
      zwi: ZipWithIndex.Aux[Anns, ZWI],
      indices: Mapper.Aux[collectFieldIndices.type, ZWI, I],
      reader: Lazy[PBProductReader[R, I]]
  ): PBMessageReader[A] = instance { (bytes: Array[Byte]) =>
    val fieldIndices = annotations.apply.zipWithIndex.map(collectFieldIndices)
    gen.from(reader.value.read(fieldIndices, bytes))
  }

}

object PBMessageReader extends PBMessageReaderImplicits {
  def apply[A: PBMessageReader]: PBMessageReader[A] = implicitly
}
