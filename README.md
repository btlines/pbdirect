[![Build status](https://api.travis-ci.org/btlines/pbdirect.svg?branch=master)](https://travis-ci.org/btlines/pbdirect)
[![codecov](https://codecov.io/gh/btlines/pbdirect/branch/master/graph/badge.svg)](https://codecov.io/gh/btlines/pbdirect)
[![License](https://img.shields.io/:license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Download](https://api.bintray.com/packages/beyondthelines/maven/pbdirect/images/download.svg) ](https://bintray.com/beyondthelines/maven/pbdirect/_latestVersion)

# PBDirect

Read/Write Scala objects directly to Protobuf with no .proto file definitions

## Context

Protobuf is a fast and efficient way to serialize data. While .proto files are great to share schema definitions between components, it is sometimes much simpler and straightforward to directly encode Scala object without using a .proto schema definition file.

PBDirect aims just that: Make it easier to serialize/deserialize into Protobuf.

## Setup

In order to use PBDirect you need to add the following lines to your `build.sbt`:

```scala
resolvers += Resolver.bintrayRepo("beyondthelines", "maven")

libraryDependencies += "beyondthelines" %% "pbdirect" % "0.0.8"
```

## Dependencies

PBDirect depends on:
 - [protobuf-java](https://developers.google.com/protocol-buffers/docs/javatutorial) the Protobuf java library (maintained by Google) 
 - [shapeless](https://github.com/milessabin/shapeless) for the generation of type-class instances
 - [cats](https://github.com/typelevel/cats) to deal with optional and repeated fields
 
## Usage

In order to use PBDirect you need to import the following:

```scala
import cats.instances.list._
import cats.instances.option._
import pbdirect._
```

*Note*: It's not recommended to use `import cats.instances.all._` as it may cause issues with implicit resolution.

## Example

### Schema definition

PBDirect serialises case classes into protobuf and there is no need for a .proto schema definition file.

```scala
case class MyMessage(
  id: Option[Int], 
  text: Option[String], 
  numbers: List[Int]
)
```

is equivalent to the following protobuf definition:

```protobuf
message MyMessage {
   optional int32  id      = 1;
   optional string text    = 2;
   repeated int32  numbers = 3;
}
```

The field numbers correspond to the order of the fields inside the case class.

### Serialization

You only need to call the `toPB` method on your case class. This method is implicitly added with `import pbdirect._`.

```scala
val message = MyMessage(
  id = Some(123),
  text = Some("Hello"),
  numbers = List(1, 2, 3, 4)
)
val bytes = message.toPB
```

### Deserialization

Deserializing bytes into a case class is also straight forward. You only need to call the `pbTo[A]` method on the byte array containing the protobuf encoded data.
This method is added implicitly on all `Array[Byte]` by importing `pbdirect._`.

```scala
val bytes: Array[Byte] = Array[Byte](8, 123, 18, 5, 72, 101, 108, 108, 111, 24, 1, 32, 2, 40, 3, 48, 4)
val message = bytes.pbTo[MyMessage]
```

## Extension

You might want to define your own formats for unsupported types.
E.g. to add a format to write `java.time.Instant` you can do:

```scala
import java.time.Instant
import cats.syntax.invariant._

implicit val instantFormat: PBFormat[Instant] =
  PBFormat[Long].imap(Instant.ofEpochMilli)(_.toEpochMilli)
```

If you only need a reader you can map over an existing `PBReader`

```scala
import java.time.Instant
import cats.syntax.functor._

implicit val instantReader: PBReader[Instant] =
  PBReader[Long].map(Instant.ofEpochMilli)
```

And for a writer you simply contramap over it:

```scala
import java.time.Instant
import cats.syntax.contravariant._

implicit val instantWriter: PBWriter[Instant] =
  PBWriter[Long].contramap(_.toEpochMilli)
  )
```

### More information

Finally you can find more implementation details [over here](http://www.beyondthelines.net/computing/pbdirect-protobuf-without-the-proto-files/)
