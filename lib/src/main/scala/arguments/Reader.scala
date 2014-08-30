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


trait Reader[T] {
  def read(x: String): T
}
object Reader {
  def reads[A](f: String ⇒ A): Reader[A] = new Reader[A] {
    def read(x: String): A = f(x)
  }

  implicit val int = reads(_.toInt)
  implicit val string = reads(identity)
  implicit val double = reads(_.toDouble)
  implicit val long = reads(_.toLong)
  implicit val bigInt = reads(BigInt(_))
  implicit val bigDecimal = reads(BigDecimal(_))
  implicit val file = reads(new java.io.File(_))
  implicit val uri = reads(new java.net.URI(_))
}
