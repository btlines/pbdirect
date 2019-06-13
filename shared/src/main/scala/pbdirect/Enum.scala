package pbdirect

import shapeless.{:+:, CNil, Coproduct, Generic, Witness}

object Enum {
  def values[T](implicit v: Values[T], ord: Ordering[T]): Seq[T] = v.apply.sorted
  def fromInt[T](index: Int)(implicit v: Values[T], ord: Ordering[T]): T = values.apply(index)
  def toInt[T](a: T)(implicit v: Values[T], ord: Ordering[T]): Int = values.indexOf(a)

  trait Values[T] {
    def apply: List[T]
  }

  object Values {
    implicit def values[A, Repr <: Coproduct](implicit gen: Generic.Aux[A, Repr], v: Aux[A, Repr]): Values[A] =
      new Values[A] { def apply = v.values }

    trait Aux[A, Repr] {
      def values: List[A]
    }

    object Aux {
      implicit def cnilAux[E]: Aux[E, CNil] = new Aux[E, CNil] { def values = Nil }
      implicit def cconsAux[E, V <: E, R <: Coproduct](implicit l: Witness.Aux[V], r: Aux[E, R]): Aux[E, V :+: R] =
        new Aux[E, V :+: R] { def values = l.value :: r.values }
    }
  }
}
