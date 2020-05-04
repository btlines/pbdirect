package pbdirect

import shapeless._

trait PBOneofFieldReader[A <: Coproduct] {
  def read(indices: List[Int], bytes: Array[Byte]): Option[A]
}

trait PBOneofFieldReaderImplicits {
  def instance[A <: Coproduct](f: (List[Int], Array[Byte]) => Option[A]): PBOneofFieldReader[A] =
    new PBOneofFieldReader[A] {
      override def read(indices: List[Int], bytes: Array[Byte]): Option[A] = f(indices, bytes)
    }

  implicit val cnilReader: PBOneofFieldReader[CNil] = instance((_, _) => None)

  implicit def cconsReader[H, T <: Coproduct](implicit
      headReader: PBFieldReader[Option[H]],
      tailReader: Lazy[PBOneofFieldReader[T]]
  ): PBOneofFieldReader[H :+: T] =
    instance { (indices: List[Int], bytes: Array[Byte]) =>
      headReader
        .read(indices.head, bytes)
        .map(Inl(_))
        .orElse(tailReader.value.read(indices.tail, bytes).map(Inr(_)))
    }

}

object PBOneofFieldReader extends PBOneofFieldReaderImplicits {
  def apply[A <: Coproduct: PBOneofFieldReader]: PBOneofFieldReader[A] = implicitly
}
