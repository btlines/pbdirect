package pbdirect

import com.google.protobuf.CodedOutputStream
import shapeless._

trait PBOneofFieldWriter[A <: Coproduct] {
  def writeTo(
      indices: List[Int],
      value: A,
      out: CodedOutputStream,
      flags: PBFieldWriter.Flags
  ): Unit
}

trait PBOneofFieldWriterImplicits {

  def instance[A <: Coproduct](
      f: (List[Int], A, CodedOutputStream, PBFieldWriter.Flags) => Unit
  ): PBOneofFieldWriter[A] =
    new PBOneofFieldWriter[A] {
      override def writeTo(
          indices: List[Int],
          value: A,
          out: CodedOutputStream,
          flags: PBFieldWriter.Flags
      ): Unit =
        f(indices, value, out, flags)
    }

  implicit val cnilWriter: PBOneofFieldWriter[CNil] =
    instance((_, _, _, _) => throw new IllegalStateException("Cannot write CNil"))

  implicit def cconsWriter[H, T <: Coproduct](implicit
      headWriter: PBFieldWriter[H],
      tailWriter: Lazy[PBOneofFieldWriter[T]]
  ): PBOneofFieldWriter[H :+: T] =
    instance {
      (indices: List[Int], value: H :+: T, out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
        value match {
          case Inl(h) =>
            // We should always write a oneof field, even if it's the default value,
            // so the reader knows which field was set
            val updatedFlags = flags.copy(skipDefaultValue = false)
            headWriter.writeTo(indices.head, h, out, updatedFlags)
          case Inr(t) =>
            tailWriter.value.writeTo(indices.tail, t, out, flags)
        }
    }

}

object PBOneofFieldWriter extends PBOneofFieldWriterImplicits {
  def apply[A <: Coproduct: PBOneofFieldWriter]: PBOneofFieldWriter[A] = implicitly
}
