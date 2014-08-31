Arguments
=========

Simple parsing of command line arguments for Scala.

## Motivation

There are already many good argument parsing libraries out there,
but I wanted to write this one mainly to explore Scala macros.
The goal is to have a macro generate the code that is required to
parse the arguments into a given type by statically examining this type,
so that one doesn't have to configure a arguments schema or something similar.
The goal is not to have a full featured, highly flexible commandline parser.


## Get it

Arguments is yet to be coded, it's just an idea for now.

## Usage

```scala
import java.io.File
import scala.concurrent.duration.Duration
import scala.util.Try
import arguments._

case class Job(target: File) extends AnyVal
case class Server(host: String = "localhost", port: Int = 8080)
case class Cli(name: String, jobs: List[Job], timeout: Duration, server: Server)

object Main extends App {
  val cli: Try[ParseResult[Cli]] = Arguments[Cli](args)
  println(cli)
}
```


```
./foo --name bar --server-host '12.34.56.78' --job /foo/bar --job /bar/baz --timeout 60s baz qux
Success(ParseResult(Cli(bar, List(Job(/foo/bar), Job(/bar/baz)), 60 seconds, Server(12.34.56.78, 8080)), List(baz, qux)))
```

### Explanation

Most arguments/options/cli parser require you to configure a parser instance or provide some kind of schema,
then you invoke this parser and extract the result to build up your options types.
With Arguments, you specify your options types upfront and they will provide the schema
by applying some rules and conventions. The rest is done by a macro.

### Conventions

- all parameters are mandatory
- the name of a parameter names serve as options long name or its base name
- the type of a parameter determines how it is translated/parsed
-- boolean parameters result in a `--{name}` flag, that can be left out
-- string and numeric parameters result in a `--{name} {arg}` option
-- java.io.File and java.net.URI parameters result in a `--{name} {arg}` option (same as string, really)
-- scala.concurrent.duration.Duration parameters result in a `--{name} {arg}` option, where the `{arg}` is parsed by the Duration constructor
-- Pairs (Tuple2) result in a `--{name} {key}={value}` option
-- List[_] parameter allow for repetition
-- Map[_, _] parameter act like repetition of pairs
-- default values allow for optional parameters
-- types that extend AnyVal are treated as value classes, that is their value type determines the result
-- case types introduce groups. their parameters are interpreted recursively, but their names are joined with the name of the case parameters by a `-`

The actual parsing is done by [scopt](https://github.com/scopt/scopt)

### TODO

- short names
- different parsing back-ends

## How it works

The basic signature is `Arguments.apply[A](args: Array[String])`.
A macro will examine `A` and generate code, according to the conventions and rules,
that will configure and execute a scopt parser for this `A`, which has to be a case class.

The generated code will use an API that can be implemented to provide different parsing implementations.
The ScoptParsingProvider is the default implementation to use. Which one will be used is determined by
an implicit lookup.

For example, for a `case class Cli(foo: String, bar: Int = 42, baz: Boolean = false)`,
a simplified code that will be generated look like:

```scala
val parser = ScoptParsingProvider[Cli]
parser.simple[String]("foo", (v, c) => c.copy(foo = v))
parser.simple[Int]("bar", (v, c) => c.copy(bar = v))
parser.bool("baz", (c) => c.copy(baz = true))
val result = parser(args, Empty[Cli])
```

The actual code is a bit more involved.
Firstly, a new type is generated, that mirrors the provided type, but wraps every property in `Option`,
so that the macro is able to provide defaults for every parameter.

Secondly, a new instance if this type is created, where some default are filled from the provided type.
Where the provided type has defaults, the generated type will have `Some(defaultValue)`, otherwise it will use `None`.

Thirdly, the parser will operate against this generated type, settings `Some(value)` for every argument.

Lastly, a new instance of the required type will be created, pulling values out of the generated type.
This will be wrapped in a `ParseResult` and in a `Try`.

This allows the macro to first parse the arguments and then construct the type without the user
having to provide some kind of an empty instance first.

In code, this looks like the following:

```scala
case class Cli$1(foo: Option[String] = None, bar: Option[Int] = Some(42), baz: Option[Boolean] = Some(false))
val parser = ScoptParsingProvider[Cli$1]
parser.simple[String]("foo", (v, c) => c.copy(foo = Some(v)))
parser.simple[Int]("bar", (v, c) => c.copy(bar = Some(v)))
parser.bool("baz", ((c) => c.copy(baz = Some(true))))
scala.util.Try({
  val result = parser(args, new Cli$1())
  ParseResult(new Cli(
    foo = result.args.foo.getOrElse(throw new IllegalArgumentException("foo is missing")),
    bar = result.args.bar.getOrElse(throw new IllegalArgumentException("bar is missing")),
    baz = result.args.baz.getOrElse(throw new IllegalArgumentException("baz is missing")))
  , result.remaining)
})
```

You can look at the generated code by using `Arguments.applyDebug[A](args)` instead of just `apply`

## License

Apache 2
