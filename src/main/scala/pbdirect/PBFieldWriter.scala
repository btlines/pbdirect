package pbdirect

import com.google.protobuf.CodedOutputStream

trait PBFieldWriter[A] {

  /**
   * @param skipDefaultValue If true, the field should not be written if it is a scalar field with the default value for its type
   */
  def writeTo(index: Int, value: A, out: CodedOutputStream, skipDefaultValue: Boolean): Unit
}

trait PBFieldWriterImplicits {

  def instance[A](f: (Int, A, CodedOutputStream, Boolean) => Unit): PBFieldWriter[A] =
    new PBFieldWriter[A] {
      override def writeTo(
          index: Int,
          value: A,
          out: CodedOutputStream,
          skipDefaultValue: Boolean): Unit =
        f(index, value, out, skipDefaultValue)
    }

  implicit def scalarWriter[A](implicit writer: PBScalarValueWriter[A]): PBFieldWriter[A] =
    instance { (index: Int, value: A, out: CodedOutputStream, skipDefaultValue: Boolean) =>
      if (skipDefaultValue && writer.isDefault(value)) {
        // skip the field
      } else {
        out.writeTag(index, writer.wireType)
        writer.writeWithoutTag(value, out)
      }
    }

  implicit def optionWriter[A](implicit writer: PBFieldWriter[A]): PBFieldWriter[Option[A]] =
    instance { (index: Int, option: Option[A], out: CodedOutputStream, skipDefaultValue: Boolean) =>
      option.foreach(v => writer.writeTo(index, v, out, skipDefaultValue))
    }

  implicit def listWriter[A](implicit writer: PBScalarValueWriter[A]): PBFieldWriter[List[A]] =
    instance { (index: Int, list: List[A], out: CodedOutputStream, skipDefaultValue: Boolean) =>
      // TODO when we support writing packed repeated fields,
      // we need to check if the list is empty and skip the field completely
      list.foreach { value =>
        out.writeTag(index, writer.wireType)
        writer.writeWithoutTag(value, out)
      }
    }

  implicit def mapWriter[K, V](
      implicit writer: PBFieldWriter[List[(K, V)]]): PBFieldWriter[Map[K, V]] =
    instance { (index: Int, value: Map[K, V], out: CodedOutputStream, skipDefaultValue: Boolean) =>
      writer.writeTo(index, value.toList, out, skipDefaultValue)
    }

  implicit def collectionMapWriter[K, V](
      implicit writer: PBFieldWriter[List[(K, V)]]): PBFieldWriter[collection.Map[K, V]] =
    instance {
      (
          index: Int,
          value: collection.Map[K, V],
          out: CodedOutputStream,
          skipDefaultValue: Boolean) =>
        writer.writeTo(index, value.toList, out, skipDefaultValue)
    }

  implicit def seqWriter[A](implicit writer: PBFieldWriter[List[A]]): PBFieldWriter[Seq[A]] =
    instance { (index: Int, value: Seq[A], out: CodedOutputStream, skipDefaultValue: Boolean) =>
      writer.writeTo(index, value.toList, out, skipDefaultValue)
    }

}

object PBFieldWriter extends PBFieldWriterImplicits {
  def apply[A: PBFieldWriter]: PBFieldWriter[A] = implicitly
}
