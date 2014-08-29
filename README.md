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
import arguments._

case class Job(target: File) extends AnyVal
case class Server(host: String = "localhost", port: Int = 8080)
case class Cli(name: String, jobs: List[Job], timeout: Duration, server: Server)

object Main extends App {
  val cli: Cli = Arguments[Cli](args)
  println(cli)
}
```


```
./foo --name bar --server-host '12.34.56.78' --job /foo/bar --job /bar/baz --timeout 60s
Cli(bar, List(Job(/foo/bar), Job(/bar/baz)), 60 seconds, Server(12.34.56.78, 8080))
```

### In depth usage

Most arguments/options/cli parser require you to configure a parser instance or provide some kind of schema,
then you invoke this parser and extract the result to build up your options types.
With Arguments, you specify your options types upfront and they will provide the schema
by applying some rules and conventions. The rest is done by a macro.

### Conventions

- all parameters are mandatory
- the name of a parameter names serve as options long name or its base name
- the type of a parameter determines how it is translated/parsed
-- boolean parameters result in a `--{name}` and a `--no-{name}` flag
-- string and numeric parameters result in a `--{name} {arg}` option
-- java.io.File parameters result in a `--{name} {arg}` option (same as string, really)
-- scala.concurrent.duration.Duration parameters result in a `--{name} {arg}` option, where the `{arg}` is parsed by the Duration constructor
-- List[_] parameter allow for repetition
-- default values allow for optional parameters
-- types that extend AnyVal are treated as value classes, that is their value type determines the result
-- case types introduce groups. their parameters are interpreted recursively, but their names are joined with the name of the case parameters by a `-`

### TODO

- conflict resulution
- short names
- backends

## License

Apache 2
