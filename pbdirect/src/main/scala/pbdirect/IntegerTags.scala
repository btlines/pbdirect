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

/**
 * A modifier to signify that an integer field is encoded as a signed Protobuf type
 * (sint32, sint64, sfixed32 or sfixed64).
 *
 * This modifier is used by attaching it to a field's type as a shapeless tag.
 * e.g.
 *
 * {{{
 * import shapeless.tag.@@
 * import pbdirect.Signed
 *
 * case class MyMessage(
 *   signedInt: Int @@ Signed,  // will be encoded as an sint32
 *   signedLong: Long @@ Signed // will be encoded as an sint64
 * )
 * }}}
 *
 * It can also be combined with the [[Fixed]] tag, e.g.
 *
 * {{{
 * case class MyMessage(
 *   fixedSignedInt: Int @@ (Signed with Fixed),  // will be encoded as an sfixed32
 *   fixedSignedLong: Long @@ (Signed with Fixed) // will be encoded as an sfixed64
 * )
 * }}}
 */
sealed trait Signed

/**
 * A modifier to signify that an integer field is encoded as an unsigned Protobuf type
 * (uint32 or uint64).
 *
 * This modifier is used by attaching it to a field's type as a shapeless tag.
 * e.g.
 *
 * {{{
 * import shapeless.tag.@@
 * import pbdirect.Unsigned
 *
 * case class MyMessage(
 *   signedInt: Int @@ Unsigned,   // will be encoded as a uint32
 *   signedLong: Long @@ Unsigned, // will be encoded as a uint64
 * )
 * }}}
 */
sealed trait Unsigned

/**
 * A modifier to signify that an integer field is encoded as a fixed-width Protobuf type
 * (fixed32, fixed64, sfixed32 or sfixed64).
 *
 * This modifier is used by attaching it to a field's type as a shapeless tag.
 * e.g.
 *
 * {{{
 * import shapeless.tag.@@
 * import pbdirect.Fixed
 *
 * case class MyMessage(
 *   signedInt: Int @@ Fixed,   // will be encoded as a fixed32
 *   signedLong: Long @@ Fixed, // will be encoded as a fixed64
 * )
 * }}}
 *
 * It can also be combined with the [[Signed]] tag, e.g.
 *
 * {{{
 * case class MyMessage(
 *   fixedSignedInt: Int @@ (Signed with Fixed),  // will be encoded as an sfixed32
 *   fixedSignedLong: Long @@ (Signed with Fixed) // will be encoded as an sfixed64
 * )
 * }}}
 */
sealed trait Fixed
