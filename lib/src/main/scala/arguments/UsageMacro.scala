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

object UsageMacro {

  def usage[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[String] = {
    import c.universe._

    object Argument {
      val apply: PartialFunction[MethodSymbol, Argument] = {
        case m if m.returnType =:= typeOf[Boolean] ⇒ new BoolArgument(m)
        case m ⇒ new SimpleArgument(m)
      }
    }
    abstract class Argument(val param: TermSymbol) {
      final def name = param.name.decodedName.toString
      final def tree(builder: TermName, default: Option[c.Tree]): c.Tree = {
        val defs: c.Tree = default match {
          case Some(d) ⇒
            q"""
                $builder ++= "(default: "
                $builder ++= $d.toString
                $builder ++= ")"
            """
          case None ⇒
            q"""
                $builder ++= "(required)"
            """
        }

        q"""
           $builder ++= "  --"
           $builder ++= $name
           $builder ++= "  "
           ${typeDef(builder)}
           $builder ++= " "
           $defs
           $builder ++= "\n"
        """
      }
      def typeDef(parser: TermName): c.Tree
    }

    final class BoolArgument(param: TermSymbol) extends Argument(param) {
      def typeDef(builder: TermName) = q""
    }

    final class SimpleArgument(param: MethodSymbol) extends Argument(param) {
      def typeDef(builder: TermName) = {
        val tpe = param.returnType.typeSymbol.name.decodedName.toString
        q"""
           $builder ++= "["
           $builder ++= $tpe
           $builder ++= "]"
        """
      }
    }

    object FreshNames {
      val builder = c.freshName(TermName("builder"))
    }

    object TypeInformation {
      def apply(tpe: Type): TypeInformation = new TypeInformation(tpe)
    }
    class TypeInformation(val tpe: Type) {
      val values: List[TermSymbol] =
        tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.flatMap(_.collect {
          case m: TermSymbol ⇒ m
        })

      val accessors: List[MethodSymbol] =
        tpe.decls.toList.collect {
          case method: MethodSymbol if method.isCaseAccessor ⇒ method
        }
    }

    object Defaults {
      def apply(info: TypeInformation): Map[String, c.Tree] = {
        val defaultValues = collectDefaults(info.tpe)
        defaultValues
      }

      private def collectDefaults(tpe: Type) = {
        val moduleSym = tpe.typeSymbol.companion
        val typeSig = moduleSym.typeSignature
        val applySym = typeSig.decl(TermName("apply"))
        val apply = applySym.asMethod
        apply.paramLists.head.map(_.asTerm).zipWithIndex.collect {
          case (p, i) if p.isParamWithDefault ⇒
            val getterName = TermName("apply$default$" + (i + 1))
            p.name.decodedName.toString → q"$moduleSym.$getterName"
        }.toMap
      }
    }

    object ParseTrees {
      def apply(info: TypeInformation, defaults: Map[String, c.Tree]): ParseTrees = {
        val arguments = info.accessors collect Argument.apply
        new ParseTrees(arguments, defaults)
      }
    }
    class ParseTrees(arguments: List[Argument], defaults: Map[String, c.Tree]) {
      def trees(parser: TermName): List[c.Tree] = arguments.map(a ⇒ a.tree(parser, defaults.get(a.name)))
    }

    object Utils {
      def fail(msg: String): Nothing =
        c.abort(c.enclosingPosition, "\n" + msg)

      def lookup(tc: Type): c.Tree = {
        val value = c.inferImplicitValue(tc)
        if (value == EmptyTree) {
          fail("Missing implicit instance of " + tc)
        }
        value
      }

      def printTree(
        t: c.Tree, label: String = "", printTypes: Boolean = false,
        printIds: Boolean = false, printOwners: Boolean = false,
        printPositions: Boolean = false, printRootPkg: Boolean = false): Unit = {
        c.info(c.enclosingPosition, label + showCode(t, printTypes, printIds, printOwners, printPositions, printRootPkg), force = false)
      }
    }

    def runMacro(implicit pTpe: WeakTypeTag[ParserProvider]): c.Expr[String] = {
      val tpe = weakTypeOf[A]
      if (!tpe.typeSymbol.isClass) {
        Utils.fail(tpe + "is not a class")
      }
      if (tpe.typeSymbol.companion == NoSymbol) {
        Utils.fail(tpe + "is not a case class or is defined locally")
      }
      val generated = generate(tpe, pTpe.tpe)
//      Utils.printTree(generated, "Generated code: \n\n")
      c.Expr[String](generated)
    }

    def generate(tpe: Type, providerTpe: Type) = {
      val tpeInfo = TypeInformation(tpe)
      val mock = Defaults(tpeInfo)
      val trees = ParseTrees(tpeInfo, mock).trees(FreshNames.builder)

      q"""
        {
          val ${FreshNames.builder} = new StringBuilder
          ${FreshNames.builder} ++= "usage"
          ${FreshNames.builder} ++= "\n"
          ..$trees
          ${FreshNames.builder}.result
        }
      """
    }

    runMacro

  }
}
