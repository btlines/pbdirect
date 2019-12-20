package pbdirect

import java.io.ByteArrayOutputStream

import cats.Contravariant
import com.google.protobuf.CodedOutputStream
import com.google.protobuf.WireFormat._
import enumeratum.values.IntEnumEntry

trait PBScalarValueWriter[A] {

  /**
   * The wire type that this writer uses.
   *
   * 0 = varint
   * 1 = fixed64
   * 2 = length delimited
   * 5 = fixed32
   * (3 and 4 are deprecated and should not be used)
   *
   * @return one of the `WIRETYPE` constants listed in [[com.google.protobuf.WireFormat]]
   */
  def wireType: Int

  /**
   * Write the value to the output stream *without* prefixing a tag.
   */
  def writeWithoutTag(value: A, out: CodedOutputStream): Unit

}

trait LowPriorityPBScalarValueWriterImplicits {
  def instance[A](theWireType: Int)(f: (A, CodedOutputStream) => Unit): PBScalarValueWriter[A] =
    new PBScalarValueWriter[A] {
      override def wireType: Int = theWireType
      override def writeWithoutTag(value: A, out: CodedOutputStream): Unit =
        f(value, out)
    }

  implicit def embeddedMessageWriter[A](
      implicit messageWriter: PBMessageWriter[A]): PBScalarValueWriter[A] =
    instance(WIRETYPE_LENGTH_DELIMITED) { (message, out) =>
      {
        val buffer    = new ByteArrayOutputStream()
        val bufferOut = CodedOutputStream.newInstance(buffer)
        messageWriter.writeTo(message, bufferOut)
        bufferOut.flush()
        out.writeByteArrayNoTag(buffer.toByteArray)
      }
    }

}

trait PBScalarValueWriterImplicits extends LowPriorityPBScalarValueWriterImplicits {

  implicit val booleanWriter: PBScalarValueWriter[Boolean] =
    instance(WIRETYPE_VARINT) { (value, out) =>
      out.writeBoolNoTag(value)
    }

  implicit val byteWriter: PBScalarValueWriter[Byte] =
    instance(WIRETYPE_VARINT) { (value, out) =>
      out.writeInt32NoTag(value.toInt)
    }

  implicit val shortWriter: PBScalarValueWriter[Short] =
    instance(WIRETYPE_VARINT) { (value, out) =>
      out.writeInt32NoTag(value.toInt)
    }

  implicit val intWriter: PBScalarValueWriter[Int] =
    instance(WIRETYPE_VARINT) { (value, out) =>
      out.writeInt32NoTag(value)
    }

  implicit val longWriter: PBScalarValueWriter[Long] =
    instance(WIRETYPE_VARINT) { (value, out) =>
      out.writeInt64NoTag(value)
    }

  implicit val floatWriter: PBScalarValueWriter[Float] =
    instance(WIRETYPE_FIXED32) { (value, out) =>
      out.writeFloatNoTag(value)
    }

  implicit val doubleWriter: PBScalarValueWriter[Double] =
    instance(WIRETYPE_FIXED64) { (value, out) =>
      out.writeDoubleNoTag(value)
    }

  implicit val stringWriter: PBScalarValueWriter[String] =
    instance(WIRETYPE_LENGTH_DELIMITED) { (value, out) =>
      out.writeStringNoTag(value)
    }

  implicit val bytesWriter: PBScalarValueWriter[Array[Byte]] =
    instance(WIRETYPE_LENGTH_DELIMITED) { (value, out) =>
      out.writeByteArrayNoTag(value)
    }

  implicit def keyValuePairWriter[K, V](
      implicit keyFieldWriter: PBFieldWriter[K],
      valueFieldWriter: PBFieldWriter[V]): PBScalarValueWriter[(K, V)] =
    instance(WIRETYPE_LENGTH_DELIMITED) { (pair, out) =>
      val buffer    = new ByteArrayOutputStream()
      val bufferOut = CodedOutputStream.newInstance(buffer)
      keyFieldWriter.writeTo(1, pair._1, bufferOut)
      valueFieldWriter.writeTo(2, pair._2, bufferOut)
      bufferOut.flush()
      out.writeByteArrayNoTag(buffer.toByteArray)
    }

  implicit def enumWriter[E](
      implicit values: Enum.Values[E],
      ordering: Ordering[E]): PBScalarValueWriter[E] =
    instance(WIRETYPE_VARINT) { (value, out) =>
      out.writeInt32NoTag(Enum.toInt(value))
    }

  implicit def enumerationWriter[E <: Enumeration#Value]: PBScalarValueWriter[E] =
    instance(WIRETYPE_VARINT) { (value, out) =>
      out.writeInt32NoTag(value.id)
    }

  implicit def enumeratumIntEnumEntryWriter[E <: IntEnumEntry]: PBScalarValueWriter[E] =
    instance(WIRETYPE_VARINT) { (entry, out) =>
      out.writeInt32NoTag(entry.value)
    }

  implicit object ContravariantWriter extends Contravariant[PBScalarValueWriter] {
    override def contramap[A, B](writer: PBScalarValueWriter[A])(f: B => A) =
      instance(writer.wireType) { (b: B, out: CodedOutputStream) =>
        writer.writeWithoutTag(f(b), out)
      }
  }
}

object PBScalarValueWriter extends PBScalarValueWriterImplicits {
  def apply[A: PBScalarValueWriter]: PBScalarValueWriter[A] = implicitly
}
