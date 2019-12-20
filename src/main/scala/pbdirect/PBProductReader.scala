package pbdirect

import shapeless._

trait PBProductReader[R <: HList, I <: HList] {
  def read(indices: I, bytes: Array[Byte]): R
}

trait PBProductReaderImplicits {

  def instance[R <: HList, I <: HList](f: (I, Array[Byte]) => R): PBProductReader[R, I] =
    new PBProductReader[R, I] {
      def read(indices: I, bytes: Array[Byte]): R = f(indices, bytes)
    }

  implicit val hnilProductReader: PBProductReader[HNil, HNil] = PBProductReader.instance {
    (indices: HNil, bytes: Array[Byte]) =>
      HNil
  }

  implicit def consProductReader[H, T <: HList, IT <: HList](
      implicit
      headFieldReader: PBFieldReader[H],
      tail: Lazy[PBProductReader[T, IT]]): PBProductReader[H :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      headFieldReader.read(indices.head.values.head, bytes) :: tail.value.read(indices.tail, bytes)
    }

}

object PBProductReader extends PBProductReaderImplicits {
  def apply[R <: HList, I <: HList](implicit reader: PBProductReader[R, I]) = reader
}
