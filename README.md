
[comment]: # (Start Badges)

[![Build Status](https://travis-ci.org/47deg/pbdirect.svg?branch=master)](https://travis-ci.org/47deg/pbdirect) [![codecov.io](http://codecov.io/gh/47deg/pbdirect/branch/master/graph/badge.svg)](http://codecov.io/gh/47deg/pbdirect) [![Maven Central](https://img.shields.io/badge/maven%20central-0.2.2-green.svg)](https://oss.sonatype.org/#nexus-search;gav~com.47deg~pbdirect*) [![Latest version](https://img.shields.io/badge/pbdirect-0.2.2-green.svg)](https://index.scala-lang.org/47deg/pbdirect) [![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://raw.githubusercontent.com/47deg/pbdirect/master/LICENSE) [![Join the chat at https://gitter.im/47deg/pbdirect](https://badges.gitter.im/47deg/pbdirect.svg)](https://gitter.im/47deg/pbdirect?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![GitHub Issues](https://img.shields.io/github/issues/47deg/pbdirect.svg)](https://github.com/47deg/pbdirect/issues)

[comment]: # (End Badges)

# PBDirect

Read/Write Scala objects directly to Protobuf with no .proto file definitions

## Context

Protobuf is a fast and efficient way to serialize data. While .proto files are great to share schema definitions between components, it is sometimes much simpler and straightforward to directly encode Scala object without using a .proto schema definition file.

PBDirect aims just that: Make it easier to serialize/deserialize into Protobuf.

## Setup

In order to use PBDirect you need to add the following lines to your `build.sbt`:

[comment]: # (Start Replace)

```scala
libraryDependencies += "com.47deg" %% "pbdirect" % "0.2.2"
```

[comment]: # (End Replace)

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

[comment]: # (Start Copyright)
# Copyright

pbdirect is designed and developed by 47 Degrees

Copyright (C) 2019 47 Degrees. <http://47deg.com>

[comment]: # (End Copyright)