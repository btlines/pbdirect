/*
 * Copyright (c) 2017-2020 47 Degrees Open Source <https://www.47deg.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

// Copyright (c) 2017-2020 47 Degrees Open Source <https://www.47deg.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
// FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
// IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package pbdirect

import shapeless.{:+:, CNil, Coproduct, Generic, Witness}

@deprecated("Please use an enumeratum IntEnum instead", since = "0.5.2")
object Enum {
  def values[T](implicit v: Values[T], ord: Ordering[T]): Seq[T]         = v.apply.sorted
  def fromInt[T](index: Int)(implicit v: Values[T], ord: Ordering[T]): T = values.apply(index)
  def toInt[T](a: T)(implicit v: Values[T], ord: Ordering[T]): Int       = values.indexOf(a)

  trait Values[T] {
    def apply: List[T]
  }

  object Values {
    implicit def values[A, Repr <: Coproduct](
        /*
         * scalac will (correctly) warn you that this parameter is never used,
         * but don't delete it! It's required to avoid diverging implicit expansion.
         */
        implicit
        gen: Generic.Aux[A, Repr],
        v: Aux[A, Repr]
    ): Values[A] =
      new Values[A] { def apply = v.values }

    trait Aux[A, Repr] {
      def values: List[A]
    }

    object Aux {
      implicit def cnilAux[E]: Aux[E, CNil] = new Aux[E, CNil] { def values = Nil }
      implicit def cconsAux[E, V <: E, R <: Coproduct](implicit
          l: Witness.Aux[V],
          r: Aux[E, R]
      ): Aux[E, V :+: R] =
        new Aux[E, V :+: R] { def values = l.value :: r.values }
    }
  }
}
