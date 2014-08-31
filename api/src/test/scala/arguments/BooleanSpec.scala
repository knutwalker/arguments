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

class BooleanSpec extends ArgsSpec {

  case class Cli(foo: Boolean = false)
  case class Cli2(foo: Boolean = true)

  test("boolean parameters") {
    val result = Arguments[Cli](Array("--foo"))
    assert(result.args.foo)
    assert(result.remaining.isEmpty)
  }

  test("boolean default params") {
    assert(!Arguments[Cli](Array()).args.foo)
    assert(Arguments[Cli2](Array()).args.foo)
  }
}
