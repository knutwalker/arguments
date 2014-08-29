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

import org.scalatest._

class ArgumentsSpec extends FunSuite {

  test("undefined parameters") {
    intercept[IllegalArgumentException] {
      Arguments[Cli](Array("--bar"))
    }
  }

  test("remaining parameters") {
    val args = Arguments[Cli](Array("foo", "bar"))
    assert(args.remaining == List("foo", "bar"))
  }

  test("boolean parameters") {
    assert(Arguments[Cli](Array("--foo")).args.foo)
    assert(!Arguments[Cli](Array()).args.foo)
  }

}
