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

import scala.util.Success

class ArgumentsSpec extends ArgsSpec {

  case class Cli()

  test("undefined parameters") {
    intercept[IllegalArgumentException] {
      Arguments[Cli](Array("--bar")).get
    }
  }

  test("remaining parameters") {
    val Success(result) = Arguments[Cli](Array("foo", "bar"))
    assert(result.remaining == List("foo", "bar"))
  }

  case class Cli2(foo: String = "foo")

  test("default values get applied") {
    val Success(result) = Arguments[Cli2](Array())
    assert(result.args.foo == "foo")
  }

  test("command line args precede default values") {
    val Success(result) = Arguments[Cli2](Array("--foo", "bar"))
    assert(result.args.foo == "bar")
  }

  case class Cli3(foo: String)

  test("missing defaults make argument mandatory") {
    val ex = intercept[IllegalArgumentException] {
      Arguments[Cli3](Array()).get
    }
    assert(ex.getMessage == "foo is missing")
  }
}
