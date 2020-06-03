
[![codecov.io](http://codecov.io/gh/47degrees/pbdirect/branch/master/graph/badge.svg)](http://codecov.io/gh/47degrees/pbdirect) [![Maven Central](https://img.shields.io/badge/maven%20central-0.5.1-green.svg)](https://oss.sonatype.org/#nexus-search;gav~com.47deg~pbdirect*) [![Latest version](https://img.shields.io/badge/pbdirect-0.5.1-green.svg)](https://index.scala-lang.org/47degrees/pbdirect) [![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://raw.githubusercontent.com/47degrees/pbdirect/master/LICENSE) [![Join the chat at https://gitter.im/47deg/pbdirect](https://badges.gitter.im/47deg/pbdirect.svg)](https://gitter.im/47deg/pbdirect?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![GitHub Issues](https://img.shields.io/github/issues/47degrees/pbdirect.svg)](https://github.com/47degrees/pbdirect/issues)

# PBDirect

Read/Write Scala objects directly to Protobuf with no .proto file definitions

## Context

Protobuf is a fast and efficient way to serialize data. While .proto files are great to share schema definitions between components, it is sometimes much simpler and straightforward to directly encode Scala object without using a .proto schema definition file.

PBDirect aims just that: Make it easier to serialize/deserialize into Protobuf.

## Setup

In order to use PBDirect you need to add the following lines to your `build.sbt`:

```scala
libraryDependencies += "com.47deg" %% "pbdirect" % "0.5.2"
```

## Dependencies

PBDirect depends on:
 - [protobuf-java](https://developers.google.com/protocol-buffers/docs/javatutorial) the Protobuf java library (maintained by Google)
 - [shapeless](https://github.com/milessabin/shapeless) for the generation of type-class instances
 - [cats](https://github.com/typelevel/cats) to deal with optional and repeated fields

## Usage

In order to use PBDirect you need to import the following:

```scala
import pbdirect._
```

## Example

### Schema definition

PBDirect serialises case classes into protobuf and there is no need for a .proto schema definition file.

```scala
case class MyMessage(
  @pbIndex(1) id: Option[Int],
  @pbIndex(3) text: Option[String],
  @pbIndex(5) numbers: List[Int]
)
```

is equivalent to the following protobuf definition:

```protobuf
message MyMessage2 {
  int32  id              = 1;
  string text            = 3;
  repeated int32 numbers = 5;
}
```

Note that the `@pbIndex` annotation is optional. If it is not present, the field's position in the case class is used
as its index. For example, an unannotated case class like:

```scala
case class MyMessage2(
  id: Option[Int],
  text: Option[String],
  numbers: List[Int]
)
```

is equivalent to the following protobuf definition:

```protobuf
message MyMessage {
  int32  id              = 1;
  string text            = 2;
  repeated int32 numbers = 3;
}
```

### Serialization

You only need to call the `toPB` method on your case class. This method is implicitly added with `import pbdirect._`.

```scala
val message = MyMessage2(
  id = Some(123),
  text = Some("Hello"),
  numbers = List(1, 2, 3, 4)
)
// message: MyMessage2 = MyMessage2(Some(123), Some("Hello"), List(1, 2, 3, 4))
val bytes = message.toPB
// bytes: Array[Byte] = Array(
//   8,
//   123,
//   18,
//   5,
//   72,
//   101,
//   108,
//   108,
//   111,
//   26,
//   4,
//   1,
//   2,
//   3,
//   4
// )
```

### Deserialization

Deserializing bytes into a case class is also straight forward. You only need to call the `pbTo[A]` method on the byte array containing the protobuf encoded data.
This method is added implicitly on all `Array[Byte]` by importing `pbdirect._`.

```scala
val bytes2: Array[Byte] = Array[Byte](8, 123, 26, 5, 72, 101, 108, 108, 111, 40, 1, 40, 2, 40, 3, 40, 4)
// bytes2: Array[Byte] = Array(
//   8,
//   123,
//   26,
//   5,
//   72,
//   101,
//   108,
//   108,
//   111,
//   40,
//   1,
//   40,
//   2,
//   40,
//   3,
//   40,
//   4
// )
val message2 = bytes2.pbTo[MyMessage2]
// message2: MyMessage2 = MyMessage2(
//   Some(123),
//   None,
//   List(72, 101, 108, 108, 111)
// )
```

## Extension

You might want to define your own formats for unsupported types.
E.g. to add a format to write `java.time.Instant` you can do:

```scala
import java.time.Instant
import cats.implicits._

implicit val instantFormat: PBFormat[Instant] =
  PBFormat[Long].imap(Instant.ofEpochMilli(_))(_.toEpochMilli)
// instantFormat: PBFormat[Instant] = pbdirect.PBFormat$$anon$1@182b4a71
```

If you only need a reader you can map over an existing `PBScalarValueReader`

```scala
import java.time.Instant

implicit val instantReader: PBScalarValueReader[Instant] =
  PBScalarValueReader[Long].map(Instant.ofEpochMilli(_))
// instantReader: PBScalarValueReader[Instant] = pbdirect.PBScalarValueReaderImplicits$FunctorReader$$anon$2@3cfbf03f
```

And for a writer you simply contramap over it:

```scala
import java.time.Instant

implicit val instantWriter: PBScalarValueWriter[Instant] =
  PBScalarValueWriter[Long].contramap(_.toEpochMilli)
// instantWriter: PBScalarValueWriter[Instant] = pbdirect.PBScalarValueWriterImplicits$ContravariantWriter$$anon$2@1a44c50d
```

## Oneof fields

pbdirect supports protobuf [`oneof` fields](https://developers.google.com/protocol-buffers/docs/proto3#oneof) encoded as [Shapeless](https://github.com/milessabin/shapeless) Coproducts.

For example:

```scala
import shapeless._

case class MyMessage3(
  @pbIndex(1) number: Int,
  @pbIndex(2,3,4) coproduct: Option[Int :+: String :+: Boolean :+: CNil]
)
```

is equivalent to the following protobuf definition:

```protobuf
message MyMessage3 {
  int32 number = 1;
  oneof coproduct {
    int32 a  = 2;
    string b = 3;
    bool c   = 4;
  }
}
```

`oneof` fields with exactly two branches can also be encoded using `Either`. For example:

```scala
case class MyMessage4(
  @pbIndex(1) number: Int,
  @pbIndex(2,3) either: Option[Either[String, Boolean]]
)
```

is equivalent to:

```protobuf
message MyMessage4 {
  int32 number = 1;
  oneof either {
    string b = 2;
    bool c   = 3;
  }
}
```

Support for `oneof` fields comes with a couple of restrictions:

* `oneof` fields must have a `@pbIndex` annotation containing the indices of each of the sub-fields
* The type of `oneof` fields must be a Coproduct (or `Either`) wrapped in `Option[_]`. This is so that pbdirect can set the value to `None` when the field is missing when reading a message from protobuf.

## Default values and missing fields

When reading a protobuf message, pbdirect needs to handle missing fields by falling back to some default value. How it does this depends on the type of field.

The following table gives some examples of how pbdirect decodes missing fields:

| Scala type | Value given to missing field |
| --- | --- |
| `Int`/`Short`/`Byte`/`Long` | `0` |
| `Double`/`Float` | `0.0` |
| `String` | `""` |
| `Array[Byte]` | empty array |
| `List[_]` | empty list |
| `Map[_, _]` | empty map |
| Scala `Enumeration` or [enumeratum](https://github.com/lloydmeta/enumeratum) `IntEnum` | the entry with value 0 |
| `Option[_]` | `None` |
| `MyMessage` | an instance of `MyMessage` with all its fields set to their default values |
| `Int :+: String :+: CNil` | (not supported) |
| `Option[Int :+: String :+: CNil]` | `None` |

If you have defined your own `PBScalarValueReader` by mapping over one of the
built-in readers, you will get whatever value is produced by the default value
of the underlying type.

For example, if your message looks like:

```scala
case class MyMessage5(instant: Instant)
```

and you use the `instantReader` defined earlier, reading a message with the `instant` field missing would result in `1970-01-01T00:00:00Z`.

## Packed repeated fields

Primitive repeated fields (ints, floats, doubles, enums and booleans) are
encoded using the protobuf packed encoding by default.

This behaviour can be overriden using the `@pbUnpacked` annotation:

```scala
case class UnpackedMessage(
  @pbUnpacked() ints: List[Int]
)
```

## Fancy integer types (signed/unsigned/fixed-width)

You can tell pbdirect that an Int/Long field should be encoded in a special way
by tagging its type with `Signed`, `Unsigned` or `Fixed`. For example:

```scala
import shapeless.tag.@@
import pbdirect.{Signed, Unsigned, Fixed}

case class IntsMessage(
  normalInt            : Int,
  signedInt            : Int @@ Signed,
  unsignedInt          : Int @@ Unsigned,
  fixedWidthInt        : Int @@ Fixed,
  fixedSignedWidthInt  : Int @@ (Signed with Fixed),
  normalLong           : Long,
  signedLong           : Long @@ Signed,
  unsignedLong         : Long @@ Unsigned,
  fixedWidthLong       : Long @@ Fixed,
  fixedSignedWidthLong : Long @@ (Signed with Fixed)
)
```

would correspond to the following Protobuf definition:


```protobuf
message IntsMessage {
  int32      normalInt              =   1;
  sint32     signedInt              =   2;
  uint32     unsignedInt            =   3;
  fixed32    fixedWidthInt          =   4;
  sfixed32   fixedSignedWidthInt    =   5;
  int64      normalLong             =   6;
  sint64     signedLong             =   7;
  uint64     unsignedLong           =   8;
  fixed64    fixedWidthLong         =   9;
  sfixed64   fixedSignedWidthLong   =   10;
}
```

You can also tag individual types inside coproducts, key and value types of maps
and element types of lists:

```scala
case class AnotherIntsMessage(
  @pbIndex(1, 2) signedIntOrNormalInt  : (Int @@ Signed) :+: Int :+: CNil,
  @pbIndex(3)    signedIntFixedLongMap : Map[Int @@ Signed, Long @@ Fixed],
  @pbIndex(4)    signedIntList         : List[Int @@ Signed]
)
```

# Copyright

pbdirect is designed and developed by 47 Degrees

Copyright (C) 2019-2020 47 Degrees. <http://47deg.com>
