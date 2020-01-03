package pbdirect

/**
 * A protobuf index, also known as the field number.
 * It holds a list of indices in order to support 'oneof' fields,
 * which are encoded as shapeless Coproducts and have a different index for each branch.
 */
final case class FieldIndex(values: List[Int])
