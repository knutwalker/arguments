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

import scala.language.higherKinds
import scala.reflect.macros.blackbox


object ParseMacro {

  def parse[A: c.WeakTypeTag](c: blackbox.Context)(args: c.Expr[Array[String]]): c.Tree = {
    import c.universe._

    def fail(msg: String) =
      c.abort(c.enclosingPosition, "\n" + msg)

    def defaultChecker(cls: ClassSymbol) = {
      val defaults = cls.primaryConstructor.asMethod.paramLists.flatMap { params ⇒
        params collect {
          case m: TermSymbol if m.isParamWithDefault ⇒ m.pos
        }
      }.toSet
      (t: TermSymbol) ⇒ t.isAccessor && defaults(t.accessed.pos)
    }

    def caseAccessors(tpe: Type) = {
      tpe.decls.toList.collect {
        case method: MethodSymbol if method.isCaseAccessor ⇒ method
      }
    }

    def boolTree(m: MethodSymbol, default: Boolean, parser: TermName) = {
      val action = q"""(_.copy(${m.name} = true))"""
      q"""
      $parser.bool(${m.name.decodedName.toString}, ${!default}, $action)
      """
    }

    def lookup(tc: Type) = {
      val empty = c.inferImplicitValue(tc)
      if (empty == EmptyTree) {
        fail("Missing implicit instance of " + tc)
      }
      empty
    }

    def generate(tpe: Type, emptyTpe: Type, providerTpe: Type): c.Tree = {
      if (!tpe.typeSymbol.isClass) {
        fail(tpe + "is not a class")
      }
      val empty = lookup(emptyTpe)
      val provider = lookup(providerTpe)
      val arguments = caseAccessors(tpe)
      val isDefault = defaultChecker(tpe.typeSymbol.asClass)
      val parser = c.freshName(TermName("parser"))

      val trees = arguments collect {
        case m if m.returnType =:= typeOf[Boolean] ⇒
          boolTree(m, isDefault(m), parser)
      }

      q"""
          val $parser = $provider[$tpe]
          ..$trees
          $parser($args, $empty.empty)
      """
    }

    def runMacro(implicit tcTpe: WeakTypeTag[Empty[A]], pTpe: WeakTypeTag[ParserProvider]): c.Tree = {
      val tpe = weakTypeOf[A]
      val tc = appliedType(tcTpe.tpe, tpe :: Nil)
      val generated = generate(tpe, tc, pTpe.tpe)
      c.info(c.enclosingPosition, "Generated code: \n\n" + showCode(generated), force = false)
      generated
    }

    runMacro
  }
}
