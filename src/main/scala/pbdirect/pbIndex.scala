package pbdirect

/**
 * An annotation that must be applied to all fields in messages
 * to tell pbdirect their index (a.k.a. field number)
 *
 * e.g.
 *
 * {{{
 * case class MyMessage(
 *   @pbIndex(1) a: Int,
 *   @pbIndex(2) b: String,
 *   @pbIndex(3, 4) c: String :+: Boolean :+: CNil // oneof field
 * )
 * }}}
 */
case class pbIndex(first: Int, more: Int*)
