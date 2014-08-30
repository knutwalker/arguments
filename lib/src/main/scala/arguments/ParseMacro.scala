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

  def parse[A: c.WeakTypeTag](c: blackbox.Context)(empty: c.Expr[A])(args: c.Expr[Array[String]]): c.Tree = {
    import c.universe._

    object Argument {
      val apply: PartialFunction[MethodSymbol, Argument] = {
        case m if m.returnType =:= typeOf[Boolean] ⇒ new BoolArgument(m)
        case m ⇒ new SimpleArgument(m)
      }
    }
    abstract class Argument(val param: TermSymbol) {
      def tree(parser: TermName, isDefault: Boolean): c.Tree
    }

    final class BoolArgument(param: TermSymbol) extends Argument(param) {
      def tree(parser: TermName, isDefault: Boolean): c.Tree = {
        val action = q"""(_.copy(${param.name} = true))"""
        q"""
        $parser.bool(${param.name.decodedName.toString}, ${!isDefault}, $action)
        """
      }
    }

    final class SimpleArgument(param: MethodSymbol) extends Argument(param) {
      def tree(parser: TermName, isDefault: Boolean): c.Tree = {
        val tpe = param.returnType
        val action = q"""((v, c) => c.copy(${param.name} = v))"""
        q"""
        $parser.simple[$tpe](${param.name.decodedName.toString}, ${!isDefault}, $action)
        """
      }
    }

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

    def lookup(tc: Type) = {
      val empty = c.inferImplicitValue(tc)
      if (empty == EmptyTree) {
        fail("Missing implicit instance of " + tc)
      }
      empty
    }

//    def hkLookup(tcCtor: Type, t: Type) = {
//      val tpe = appliedType(tcCtor, t :: Nil)
//      lookup(tpe)
//    }

    def generate(tpe: Type, providerTpe: Type): c.Tree = {
      if (!tpe.typeSymbol.isClass) {
        fail(tpe + "is not a class")
      }
      val provider = lookup(providerTpe)
      val isDefault = defaultChecker(tpe.typeSymbol.asClass)
      val arguments = caseAccessors(tpe) collect Argument.apply
      val parser = c.freshName(TermName("parser"))
      val trees = arguments.map(a ⇒ a.tree(parser, isDefault(a.param)))

      q"""
          val $parser = $provider[$tpe]
          ..$trees
          $parser($args, $empty)
      """
    }

    def runMacro(implicit pTpe: WeakTypeTag[ParserProvider]): c.Tree = {
      val tpe = weakTypeOf[A]
      val generated = generate(tpe, pTpe.tpe)
      c.info(c.enclosingPosition, "Generated code: \n\n" + showCode(generated), force = false)
      generated
    }

    runMacro
  }
}
