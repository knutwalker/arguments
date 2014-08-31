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

import scala.util.Success

class SimpleTypesSpec extends ArgsSpec {

  case class Cli(foo: String = "")
  test("string parsing") {
    val Success(result) = Arguments[Cli](Array("--foo", "bar"))
    assert(result.args.foo == "bar")
    assert(result.remaining.isEmpty)
  }

  case class Cli2(foo: Int = 42)
  test("int parsing") {
    val Success(result) = Arguments[Cli2](Array("--foo", "1337"))
    assert(result.args.foo == 1337)
    assert(result.remaining.isEmpty)
  }

  case class Cli3(foo: Int = 42)
  test("malformed int") {
    intercept[IllegalArgumentException] {
      Arguments[Cli3](Array("--foo", "foo")).get
    }
  }

  case class Cli4(foo: Double = 13.37)
  test("double parsing") {
    val Success(result) = Arguments[Cli4](Array("--foo", "42"))
    assert(result.args.foo == 42.0)
    assert(result.remaining.isEmpty)
  }

  case class Cli5(foo: Double = 13.37)
  test("malformed double") {
    intercept[IllegalArgumentException] {
      Arguments[Cli5](Array("--foo", "foo")).get
    }
  }

  case class Cli6(foo: Long = 314L)
  test("long parsing") {
    val Success(result) = Arguments[Cli6](Array("--foo", "628"))
    assert(result.args.foo == 628)
    assert(result.remaining.isEmpty)
  }

  case class Cli7(foo: Long = 314L)
  test("malformed long") {
    intercept[IllegalArgumentException] {
      Arguments[Cli7](Array("--foo", "foo")).get
    }
  }

  case class Cli8(foo: BigInt = BigInt(1))
  test("bigint parsing") {
    val Success(result) = Arguments[Cli8](Array("--foo", "12"))
    assert(result.args.foo == BigInt(12))
    assert(result.remaining.isEmpty)
  }

  case class Cli9(foo: BigInt = BigInt(1))
  test("malformed bigint") {
    intercept[IllegalArgumentException] {
      Arguments[Cli9](Array("--foo", "foo")).get
    }
  }

  case class Cli10(foo: BigDecimal = BigDecimal(0.9))
  test("bigdecimal parsing") {
    val Success(result) = Arguments[Cli10](Array("--foo", "34.56"))
    assert(result.args.foo == BigDecimal(34.56))
    assert(result.remaining.isEmpty)
  }

  case class Cli11(foo: BigDecimal = BigDecimal(0.9))
  test("malformed bigdecimal") {
    intercept[IllegalArgumentException] {
      Arguments[Cli11](Array("--foo", "foo")).get
    }
  }

  case class Cli12(foo: File = new File(""))
  test("file parsing") {
    val Success(result) = Arguments[Cli12](Array("--foo", "/a/b/c"))
    assert(result.args.foo.getPath == "/a/b/c")
    assert(result.remaining.isEmpty)
  }

  case class Cli13(foo: URI = new URI(""))
  test("uri parsing") {
    val Success(result) =
      Arguments[Cli13](Array("--foo", "foo.bar://bernd@12.34.56.78:9876/a/b/c?foo=bar&baz=qux#baz"))
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

  case class Cli14(
    foo: String = "", bar: Int = 42, baz: Double = 13.37, qux: Long = 314L,
    qax: BigInt = BigInt(1), crux: BigDecimal = BigDecimal(0.9))
  test("defaults") {
    val Success(result) = Arguments[Cli14](Array())
    assert(result.args.foo == "")
    assert(result.args.bar == 42)
    assert(result.args.baz == 13.37)
    assert(result.args.qux == 314L)
    assert(result.args.qax == BigInt(1))
    assert(result.args.crux == BigDecimal(0.9))
  }

}
