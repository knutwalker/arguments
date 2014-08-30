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

class SimpleTypesSpec extends ArgsSpec {

  case class Cli(foo: String = "", bar: Int = 42)

  test("string parsing") {
    val result = Arguments(Cli())(Array("--foo", "bar"))
    assert(result.args.foo == "bar")
    assert(result.remaining.isEmpty)
  }

  test("int parsing") {
    val result = Arguments(Cli())(Array("--bar", "1337"))
    assert(result.args.bar == 1337)
    assert(result.remaining.isEmpty)
  }

  test("malformed int") {
    intercept[IllegalArgumentException] {
      Arguments(Cli())(Array("--bar", "foo"))
    }
  }

  test("defaults") {
    val result = Arguments(Cli())(Array())
    assert(result.args.foo == "")
    assert(result.args.bar == 42)
  }

}
