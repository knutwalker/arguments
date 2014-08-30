/*
 * Copyright 2014 Paul Horn
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package arguments

import java.io.File
import java.net.URI

class SimpleTypesSpec extends ArgsSpec {

  test("string parsing") {
    case class Cli(foo: String = "")
    val result = Arguments(Cli())(Array("--foo", "bar"))
    assert(result.args.foo == "bar")
    assert(result.remaining.isEmpty)
  }

  test("int parsing") {
    case class Cli(foo: Int = 42)
    val result = Arguments(Cli())(Array("--foo", "1337"))
    assert(result.args.foo == 1337)
    assert(result.remaining.isEmpty)
  }

  test("malformed int") {
    case class Cli(foo: Int = 42)
    intercept[IllegalArgumentException] {
      Arguments(Cli())(Array("--foo", "foo"))
    }
  }

  test("double parsing") {
    case class Cli(foo: Double = 13.37)
    val result = Arguments(Cli())(Array("--foo", "42"))
    assert(result.args.foo == 42.0)
    assert(result.remaining.isEmpty)
  }

  test("malformed double") {
    case class Cli(foo: Double = 13.37)
    intercept[IllegalArgumentException] {
      Arguments(Cli())(Array("--foo", "foo"))
    }
  }

  test("long parsing") {
    case class Cli(foo: Long = 314L)
    val result = Arguments(Cli())(Array("--foo", "628"))
    assert(result.args.foo == 628)
    assert(result.remaining.isEmpty)
  }

  test("malformed long") {
    case class Cli(foo: Long = 314L)
    intercept[IllegalArgumentException] {
      Arguments(Cli())(Array("--foo", "foo"))
    }
  }

  test("bigint parsing") {
    case class Cli(foo: BigInt = BigInt(1))
    val result = Arguments(Cli())(Array("--foo", "12"))
    assert(result.args.foo == BigInt(12))
    assert(result.remaining.isEmpty)
  }

  test("malformed bigint") {
    case class Cli(foo: BigInt = BigInt(1))
    intercept[IllegalArgumentException] {
      Arguments(Cli())(Array("--foo", "foo"))
    }
  }

  test("bigdecimal parsing") {
    case class Cli(foo: BigDecimal = BigDecimal(0.9))
    val result = Arguments(Cli())(Array("--foo", "34.56"))
    assert(result.args.foo == BigDecimal(34.56))
    assert(result.remaining.isEmpty)
  }

  test("malformed bigdecimal") {
    case class Cli(foo: BigDecimal = BigDecimal(0.9))
    intercept[IllegalArgumentException] {
      Arguments(Cli())(Array("--foo", "foo"))
    }
  }

  test("file parsing") {
    case class Cli(foo: File = new File(""))
    val result = Arguments(Cli())(Array("--foo", "/a/b/c"))
    assert(result.args.foo.getPath == "/a/b/c")
    assert(result.remaining.isEmpty)
  }

  test("uri parsing") {
    case class Cli(foo: URI = new URI(""))
    val result = Arguments(Cli())(
      Array("--foo", "foo.bar://bernd@12.34.56.78:9876/a/b/c?foo=bar&baz=qux#baz"))
    assert(result.args.foo.getScheme == "foo.bar")
    assert(result.args.foo.getAuthority == "bernd@12.34.56.78:9876")
    assert(result.args.foo.getUserInfo == "bernd")
    assert(result.args.foo.getHost == "12.34.56.78")
    assert(result.args.foo.getPort == 9876)
    assert(result.args.foo.getPath == "/a/b/c")
    assert(result.args.foo.getQuery == "foo=bar&baz=qux")
    assert(result.args.foo.getFragment == "baz")
    assert(result.remaining.isEmpty)
  }

  test("defaults") {
    case class Cli(
      foo: String = "", bar: Int = 42, baz: Double = 13.37, qux: Long = 314L,
      qax: BigInt = BigInt(1), crux: BigDecimal = BigDecimal(0.9))

    val result = Arguments(Cli())(Array())

    assert(result.args.foo == "")
    assert(result.args.bar == 42)
    assert(result.args.baz == 13.37)
    assert(result.args.qux == 314L)
    assert(result.args.qax == BigInt(1))
    assert(result.args.crux == BigDecimal(0.9))
  }

}
