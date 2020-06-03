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

@deprecated("Please use an enumeratum IntEnum instead", since = "0.5.2")
trait Pos {
  val _pos: Int
}

object Pos {
  implicit def posOrdering[P <: Pos]: Ordering[P] =
    new Ordering[P] {
      override def compare(x: P, y: P): Int = x._pos - y._pos
    }

  trait _0  extends Pos { override val _pos = 0  }
  trait _1  extends Pos { override val _pos = 1  }
  trait _2  extends Pos { override val _pos = 2  }
  trait _3  extends Pos { override val _pos = 3  }
  trait _4  extends Pos { override val _pos = 4  }
  trait _5  extends Pos { override val _pos = 5  }
  trait _6  extends Pos { override val _pos = 6  }
  trait _7  extends Pos { override val _pos = 7  }
  trait _8  extends Pos { override val _pos = 8  }
  trait _9  extends Pos { override val _pos = 9  }
  trait _10 extends Pos { override val _pos = 10 }
  trait _11 extends Pos { override val _pos = 11 }
  trait _12 extends Pos { override val _pos = 12 }
  trait _13 extends Pos { override val _pos = 13 }
  trait _14 extends Pos { override val _pos = 14 }
  trait _15 extends Pos { override val _pos = 15 }
  trait _16 extends Pos { override val _pos = 16 }
  trait _17 extends Pos { override val _pos = 17 }
  trait _18 extends Pos { override val _pos = 18 }
  trait _19 extends Pos { override val _pos = 19 }
  trait _20 extends Pos { override val _pos = 20 }
  trait _21 extends Pos { override val _pos = 21 }
  trait _22 extends Pos { override val _pos = 22 }
  trait _23 extends Pos { override val _pos = 23 }
  trait _24 extends Pos { override val _pos = 24 }
  trait _25 extends Pos { override val _pos = 25 }
  trait _26 extends Pos { override val _pos = 26 }
  trait _27 extends Pos { override val _pos = 27 }
  trait _28 extends Pos { override val _pos = 28 }
  trait _29 extends Pos { override val _pos = 29 }
  trait _30 extends Pos { override val _pos = 30 }
  trait _31 extends Pos { override val _pos = 31 }
  trait _32 extends Pos { override val _pos = 32 }
  trait _33 extends Pos { override val _pos = 33 }
  trait _34 extends Pos { override val _pos = 34 }
  trait _35 extends Pos { override val _pos = 35 }
  trait _36 extends Pos { override val _pos = 36 }
  trait _37 extends Pos { override val _pos = 37 }
  trait _38 extends Pos { override val _pos = 38 }
  trait _39 extends Pos { override val _pos = 39 }
  trait _40 extends Pos { override val _pos = 40 }
  trait _41 extends Pos { override val _pos = 41 }
  trait _42 extends Pos { override val _pos = 42 }
  trait _43 extends Pos { override val _pos = 43 }
  trait _44 extends Pos { override val _pos = 44 }
  trait _45 extends Pos { override val _pos = 45 }
  trait _46 extends Pos { override val _pos = 46 }
  trait _47 extends Pos { override val _pos = 47 }
  trait _48 extends Pos { override val _pos = 48 }
  trait _49 extends Pos { override val _pos = 49 }
  trait _50 extends Pos { override val _pos = 50 }
  trait _51 extends Pos { override val _pos = 51 }
  trait _52 extends Pos { override val _pos = 52 }
  trait _53 extends Pos { override val _pos = 53 }
  trait _54 extends Pos { override val _pos = 54 }
  trait _55 extends Pos { override val _pos = 55 }
  trait _56 extends Pos { override val _pos = 56 }
  trait _57 extends Pos { override val _pos = 57 }
  trait _58 extends Pos { override val _pos = 58 }
  trait _59 extends Pos { override val _pos = 59 }
  trait _60 extends Pos { override val _pos = 60 }
  trait _61 extends Pos { override val _pos = 61 }
  trait _62 extends Pos { override val _pos = 62 }
  trait _63 extends Pos { override val _pos = 63 }
  trait _64 extends Pos { override val _pos = 64 }
  trait _65 extends Pos { override val _pos = 65 }
  trait _66 extends Pos { override val _pos = 66 }
  trait _67 extends Pos { override val _pos = 67 }
  trait _68 extends Pos { override val _pos = 68 }
  trait _69 extends Pos { override val _pos = 69 }
  trait _70 extends Pos { override val _pos = 70 }
  trait _71 extends Pos { override val _pos = 71 }
  trait _72 extends Pos { override val _pos = 72 }
  trait _73 extends Pos { override val _pos = 73 }
  trait _74 extends Pos { override val _pos = 74 }
  trait _75 extends Pos { override val _pos = 75 }
  trait _76 extends Pos { override val _pos = 76 }
  trait _77 extends Pos { override val _pos = 77 }
  trait _78 extends Pos { override val _pos = 78 }
  trait _79 extends Pos { override val _pos = 79 }
  trait _80 extends Pos { override val _pos = 80 }
  trait _81 extends Pos { override val _pos = 81 }
  trait _82 extends Pos { override val _pos = 82 }
  trait _83 extends Pos { override val _pos = 83 }
  trait _84 extends Pos { override val _pos = 84 }
  trait _85 extends Pos { override val _pos = 85 }
  trait _86 extends Pos { override val _pos = 86 }
  trait _87 extends Pos { override val _pos = 87 }
  trait _88 extends Pos { override val _pos = 88 }
  trait _89 extends Pos { override val _pos = 89 }
  trait _90 extends Pos { override val _pos = 90 }
  trait _91 extends Pos { override val _pos = 91 }
  trait _92 extends Pos { override val _pos = 92 }
  trait _93 extends Pos { override val _pos = 93 }
  trait _94 extends Pos { override val _pos = 94 }
  trait _95 extends Pos { override val _pos = 95 }
  trait _96 extends Pos { override val _pos = 96 }
  trait _97 extends Pos { override val _pos = 97 }
  trait _98 extends Pos { override val _pos = 98 }
  trait _99 extends Pos { override val _pos = 99 }
}
