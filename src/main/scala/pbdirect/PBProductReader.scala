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

  implicit def hconsProductReader[H, T <: HList, IT <: HList](
      implicit
      head: PBFieldReader[H],
      tail: Lazy[PBProductReader[T, IT]]
  ): PBProductReader[H :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      head.read(indices.head.values.head, bytes) :: tail.value.read(indices.tail, bytes)
    }

  implicit def hconsCoproductOneofProductReader[H <: Coproduct, T <: HList, IT <: HList](
      implicit
      head: PBOneofFieldReader[H],
      tail: Lazy[PBProductReader[T, IT]]
  ): PBProductReader[Option[H] :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      head.read(indices.head.values, bytes) :: tail.value.read(indices.tail, bytes)
    }

  // read an Either[A, B] by treating it as a Coproduct (Left[A, B] :+: Right[A, B] :+: CNil)
  implicit def hconsEitherOneofProductReader[A, B, H <: Coproduct, T <: HList, IT <: HList](
      implicit
      gen: Generic.Aux[Either[A, B], H],
      productReader: PBProductReader[Option[H] :: T, FieldIndex :: IT]
  ): PBProductReader[Option[Either[A, B]] :: T, FieldIndex :: IT] =
    PBProductReader.instance { (indices: FieldIndex :: IT, bytes: Array[Byte]) =>
      val result = productReader.read(indices, bytes)
      result.head.map(gen.from) :: result.tail
    }

}

object PBProductReader extends PBProductReaderImplicits {
  def apply[R <: HList, I <: HList](implicit reader: PBProductReader[R, I]) = reader
}
