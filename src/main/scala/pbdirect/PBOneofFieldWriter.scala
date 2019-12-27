package pbdirect

import com.google.protobuf.CodedOutputStream
import shapeless._

trait PBOneofFieldWriter[A <: Coproduct] {
  def writeTo(indices: List[Int], value: A, out: CodedOutputStream): Unit
}

trait PBOneofFieldWriterImplicits {

  def instance[A <: Coproduct](
      f: (List[Int], A, CodedOutputStream) => Unit): PBOneofFieldWriter[A] =
    new PBOneofFieldWriter[A] {
      override def writeTo(indices: List[Int], value: A, out: CodedOutputStream): Unit =
        f(indices, value, out)
    }

  implicit val cnilWriter: PBOneofFieldWriter[CNil] = instance(
    (_, _, _) => throw new IllegalStateException("Cannot write CNil"))

  implicit def cconsWriter[H, T <: Coproduct](
      implicit
      headWriter: PBFieldWriter[H],
      tailWriter: Lazy[PBOneofFieldWriter[T]]): PBOneofFieldWriter[H :+: T] =
    instance { (indices: List[Int], value: H :+: T, out: CodedOutputStream) =>
      value match {
        case Inl(h) => headWriter.writeTo(indices.head, h, out, skipDefaultValue = false)
        case Inr(t) => tailWriter.value.writeTo(indices.tail, t, out)
      }
    }

}

object PBOneofFieldWriter extends PBOneofFieldWriterImplicits {
  def apply[A <: Coproduct: PBOneofFieldWriter]: PBOneofFieldWriter[A] = implicitly
}
