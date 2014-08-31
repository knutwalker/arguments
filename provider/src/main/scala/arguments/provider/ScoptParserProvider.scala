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

package arguments.provider

import arguments.{Reader, Parser, ParserProvider, ParseResult}
import _root_.scopt.OptionParser
import _root_.scopt.Read

trait ScoptParserProvider {

  private final class ScoptParserProvider extends ParserProvider {
    def apply[A]: Parser[A] = new ScoptParser[A]
  }

  private final class ScoptParser[A] extends Parser[A] {
    private val parser: OptionParser[ParseResult[A]] =
      new OptionParser[ParseResult[A]]("") {
        override val showUsageOnError = false
      }

    def bool(name: String, f: (A) ⇒ A): Unit = {
      parser.opt[Unit](name) optional() action((_, pr) ⇒ pr.copy(args = f(pr.args)))
    }

    def simple[B](name: String, f: (B, A) ⇒ A)(implicit B: Reader[B]): Unit = {
      parser.opt[B](name)(Read.reads(B.read)) optional() action((x, pr) ⇒ pr.copy(args = f(x, pr.args)))
    }

    def apply(args: Array[String], empty: A): ParseResult[A] = {
      parser.arg[String]("remainder") optional() unbounded() action { (x, c) ⇒
        c.copy(remaining = c.remaining :+ x)
      }
      parser.parse(args, ParseResult(empty, List())) match {
        case Some(x) ⇒ x
        case None    ⇒ throw new IllegalArgumentException()
      }
    }
  }

  implicit val scopt: ParserProvider = new ScoptParserProvider
}

