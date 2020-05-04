package pbdirect

import com.google.protobuf.CodedOutputStream
import java.io.ByteArrayOutputStream

trait PBFieldWriter[A] {

  def writeTo(index: Int, value: A, out: CodedOutputStream, flags: PBFieldWriter.Flags): Unit

}

trait PBFieldWriterImplicits {

  def instance[A](f: (Int, A, CodedOutputStream, PBFieldWriter.Flags) => Unit): PBFieldWriter[A] =
    new PBFieldWriter[A] {
      override def writeTo(
          index: Int,
          value: A,
          out: CodedOutputStream,
          flags: PBFieldWriter.Flags
      ): Unit =
        f(index, value, out, flags)
    }

  implicit def scalarWriter[A](implicit writer: PBScalarValueWriter[A]): PBFieldWriter[A] =
    instance { (index: Int, value: A, out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
      if (flags.skipDefaultValue && writer.isDefault(value)) {
        // skip the field
      } else {
        out.writeTag(index, writer.wireType)
        writer.writeWithoutTag(value, out)
      }
    }

  implicit def optionWriter[A](implicit writer: PBFieldWriter[A]): PBFieldWriter[Option[A]] =
    instance {
      (index: Int, option: Option[A], out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
        option.foreach(v => writer.writeTo(index, v, out, flags))
    }

  implicit def listWriter[A](implicit writer: PBScalarValueWriter[A]): PBFieldWriter[List[A]] =
    instance { (index: Int, list: List[A], out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
      if (writer.canBePacked && !flags.unpacked) {
        if (list.nonEmpty) {
          val buffer    = new ByteArrayOutputStream()
          val packedOut = CodedOutputStream.newInstance(buffer)
          list.foreach(value => writer.writeWithoutTag(value, packedOut))
          packedOut.flush()
          out.writeByteArray(index, buffer.toByteArray)
        }
      } else {
        list.foreach { value =>
          out.writeTag(index, writer.wireType)
          writer.writeWithoutTag(value, out)
        }
      }
    }

  implicit def mapWriter[K, V](implicit
      writer: PBFieldWriter[List[(K, V)]]
  ): PBFieldWriter[Map[K, V]] =
    instance { (index: Int, value: Map[K, V], out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
      writer.writeTo(index, value.toList, out, flags)
    }

  implicit def collectionMapWriter[K, V](implicit
      writer: PBFieldWriter[List[(K, V)]]
  ): PBFieldWriter[collection.Map[K, V]] =
    instance {
      (
          index: Int,
          value: collection.Map[K, V],
          out: CodedOutputStream,
          flags: PBFieldWriter.Flags
      ) => writer.writeTo(index, value.toList, out, flags)
    }

  implicit def seqWriter[A](implicit writer: PBFieldWriter[List[A]]): PBFieldWriter[Seq[A]] =
    instance { (index: Int, value: Seq[A], out: CodedOutputStream, flags: PBFieldWriter.Flags) =>
      writer.writeTo(index, value.toList, out, flags)
    }

}

object PBFieldWriter extends PBFieldWriterImplicits {

  def apply[A: PBFieldWriter]: PBFieldWriter[A] = implicitly

  /**
   * @param skipDefaultValue If true, the field should not be written if it is a scalar field with the default value for its type
   * @param unpacked If true, the field should be written using unpacked encoding if it is a primitive repeated field
   */
  case class Flags(
      skipDefaultValue: Boolean,
      unpacked: Boolean
  )

}
