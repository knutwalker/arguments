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

case class Cli(foo: Boolean = false, bar: Int = 0)

class Arguments[T] private(val args: T, val remaining: List[String]) {
  override def toString: String = args.toString
}

object Arguments {
  def apply[T](args: Array[String])(implicit ev: T =:= Cli): Arguments[Cli] = {
    case class Foo(cli: Cli, rem: List[String])
    val parser = new scopt.OptionParser[Foo]("foo") {
      override val showUsageOnError = false

      opt[Unit]("foo") action { (x, c) ⇒
        c.copy(cli = c.cli.copy(foo = true))
      }
      arg[String]("remainder") optional() unbounded() action { (x, c) ⇒
        c.copy(rem = c.rem :+ x)
      }
    }
    parser.parse(args, Foo(Cli(), List())) match {
      case Some(x) ⇒ new Arguments(x.cli, x.rem)
      case None    ⇒ throw new IllegalArgumentException()
    }
  }
}
