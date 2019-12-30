package pbdirect

/**
 * An annotation to specify that a primitive repeated field
 * should **not** be encoded using the protobuf packed encoding.
 *
 * e.g.
 *
 * {{{
 * case class MyMessage(
 *   @pbUnpacked() numbers: List[Int]
 * )
 * }}}
 *
 * Unless annotated with @pbUnpacked, all primitive repeated fields
 * (ints, floats, booleans and enums) will be packed by default.
 * This is the standard proto3 behaviour.
 *
 */
case class pbUnpacked()
