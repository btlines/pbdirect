package pbdirect

import com.google.protobuf.CodedInputStream

trait PBFieldReader[A] {
  def read(index: Int, bytes: Array[Byte]): A
}

trait PBFieldReaderImplicits {
  def instance[A](f: (Int, Array[Byte]) => A): PBFieldReader[A] = new PBFieldReader[A] {
    override def read(index: Int, bytes: Array[Byte]): A = f(index, bytes)
  }

  implicit def repeatedFieldReader[A](
      implicit reader: PBScalarValueReader[A]): PBFieldReader[List[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                          => done = true
          case tag if (tag >> 3) == index => as ::= reader.read(input)
          case tag                        => input.skipField(tag)
        }
      }
      as.reverse
    }

  implicit def requiredFieldReader[A](implicit reader: PBScalarValueReader[A]): PBFieldReader[A] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                          => done = true
          case tag if (tag >> 3) == index => as ::= reader.read(input)
          case tag                        => input.skipField(tag)
        }
      }
      as.headOption.getOrElse(reader.defaultValue)
    }

  implicit def optionalFieldReader[A](
      implicit reader: PBFieldReader[List[A]]): PBFieldReader[Option[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      reader.read(index, bytes).lastOption
    }

  implicit def mapFieldReader[K, V](
      implicit reader: PBFieldReader[List[(K, V)]]): PBFieldReader[Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      reader.read(index, bytes).toMap
    }

  implicit def collectionMapFieldReader[K, V](
      implicit reader: PBFieldReader[List[(K, V)]]): PBFieldReader[collection.Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      reader.read(index, bytes).toMap
    }

  implicit def seqFieldReader[A](implicit reader: PBFieldReader[List[A]]): PBFieldReader[Seq[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      reader.read(index, bytes)
    }

}

object PBFieldReader extends PBFieldReaderImplicits {
  def apply[A: PBFieldReader]: PBFieldReader[A] = implicitly
}
