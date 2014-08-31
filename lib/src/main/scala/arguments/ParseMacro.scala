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

  def parse[A: c.WeakTypeTag](c: blackbox.Context)(args: c.Expr[Array[String]]): c.Expr[ParseResult[A]] = {
    import c.universe._

    object Argument {
      val apply: PartialFunction[MethodSymbol, Argument] = {
        case m if m.returnType =:= typeOf[Boolean] ⇒ new BoolArgument(m)
        case m ⇒ new SimpleArgument(m)
      }
    }
    abstract class Argument(val param: TermSymbol) {
      def tree(parser: TermName): c.Tree
    }

    final class BoolArgument(param: TermSymbol) extends Argument(param) {
      def tree(parser: TermName): c.Tree = {
        val action = q"""(_.copy(${param.name} = Some(true)))"""
        q"""
        $parser.bool(${param.name.decodedName.toString}, $action)
        """
      }
    }

    final class SimpleArgument(param: MethodSymbol) extends Argument(param) {
      def tree(parser: TermName): c.Tree = {
        val tpe = param.returnType
        val action = q"""((v, c) => c.copy(${param.name} = Some(v)))"""
        q"""
        $parser.simple[$tpe](${param.name.decodedName.toString}, $action)
        """
      }
    }

    object FreshNames {
      val parser = c.freshName(TermName("parser"))
      val parseResult = c.freshName(TermName("parseResult"))
    }

    object TypeInformation {
      def apply(tpe: Type): TypeInformation = new TypeInformation(tpe)
    }
    class TypeInformation(val tpe: Type) {
      val values: List[TermSymbol] =
        tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.flatMap(_.collect {
          case m: TermSymbol ⇒ m
        })

      val methods: List[MethodSymbol] =
        tpe.decls.toList.collect {
          case method: MethodSymbol ⇒ method
        }

      val accessors: List[MethodSymbol] =
        methods.filter(_.isCaseAccessor)
    }

    object MockClass {
      def apply(info: TypeInformation): MockClass = {
        val optTpes = optParams(info.tpe, info.accessors)
        val defaultValues = collectDefaults(info)
        mockType(info.tpe, optTpes, defaultValues)
      }

      private def optParams(tpe: Type, parameters: List[MethodSymbol]) = {
        val optTpe = weakTypeOf[Option[_]]
        parameters map { m ⇒
          m.name.toTermName → appliedType(optTpe, m.returnType :: Nil)
        }
      }

      private def collectDefaults(info: TypeInformation) = {
        val moduleSym = info.tpe.typeSymbol.companion
        val typeSig = moduleSym.typeSignature
        val applySym = typeSig.decl(TermName("apply"))
        val apply = applySym.asMethod
        apply.paramLists.head.map(_.asTerm).zipWithIndex.collect {
          case (p, i) if p.isParamWithDefault ⇒
            val getterName = TermName("apply$default$" + (i + 1))
            p.name.decodedName.toString → q"$moduleSym.$getterName"
        }.toMap
      }

//      def collectFromLocal = {
//        //      private def collectDefaults(tpe: Type, methods: List[TermSymbol], values: List[TermSymbol]) = {
//        //        @tailrec
//        //        def loop(defaults: List[TermSymbol], values: List[TermSymbol], result: List[(String, c.Tree)]): List[(String, c.Tree)] = defaults match {
//        //          case df :: rest if df.isParamWithDefault ⇒
//        //            values match {
//        //              case vl :: restValues if vl.isParamWithDefault ⇒
//        //                val tpeApplication = q"${tpe.typeSymbol.asClass}.${df.asTerm.name}"
//        //                loop(rest, restValues, (vl.name.decodedName.toString, tpeApplication) :: result)
//        //              case vl :: restValues ⇒
//        //                loop(defaults, restValues, result)
//        //              case _ ⇒ result
//        //            }
//        //          case df :: rest ⇒
//        //            loop(rest, values, result)
//        //          case _ ⇒
//        //            result
//        //        }
//        //
//        //        loop(methods, values, List()).toMap
//        //      }
//        info.values.zipWithIndex.collect {
//          case (m, i) if m.isParamWithDefault ⇒
//            val name = m.name.decodedName.toString
//            val getter = TermName("copy$default$" + (i + 1))
//            name → q"${info.tpe.typeSymbol}.$getter"
//        }.toMap
//      }

      private def mockType(tpe: Type, optTpes: List[(TermName, Type)], defaults: Map[String, c.Tree]) = {
        val tpeName = c.freshName(tpe.typeSymbol.name.toTypeName)
        val defs = defaults.mapValues(t ⇒ q"Some($t)").withDefault(_ ⇒ q"None")
        val ccTps = optTpes map {
          case (name, t) ⇒ q"val $name: $t = ${defs(name.decodedName.toString)}"
        }
        new MockClass(tpeName, ccTps)
      }
    }
    class MockClass(val name: TypeName, params: List[c.Tree]) {
      val tpe: c.Tree = tq"$name"
      val cls: c.Tree = q"case class $name(..$params)"
      val empty: c.Tree = q"new $name()"
    }

    object ParseTrees {
      def apply(info: TypeInformation): ParseTrees = {
        val arguments = info.accessors collect Argument.apply
        new ParseTrees(arguments)
      }
    }
    class ParseTrees(arguments: List[Argument]) {
      def trees(parser: TermName): List[c.Tree] = arguments.map(_.tree(parser))
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

    def runMacro(implicit pTpe: WeakTypeTag[ParserProvider]): c.Expr[ParseResult[A]] = {
      val tpe = weakTypeOf[A]
      if (!tpe.typeSymbol.isClass) {
        Utils.fail(tpe + "is not a class")
      }
      if (tpe.typeSymbol.companion == NoSymbol) {
        Utils.fail(tpe + "is not a case class or is defined locally")
      }
      val generated = generate(tpe, pTpe.tpe)
//      Utils.printTree(generated, "Generated code: \n\n")
      c.Expr[ParseResult[A]](generated)
    }

    def generate(tpe: Type, providerTpe: Type) = {
      val provider = Utils.lookup(providerTpe)
      val tpeInfo = TypeInformation(tpe)
      val mock = MockClass(tpeInfo)
      val trees = ParseTrees(tpeInfo).trees(FreshNames.parser)
      val inst = newInstance(tpeInfo)

      q"""
        {
          ${mock.cls}
          val ${FreshNames.parser} = $provider[${mock.tpe}]
          ..$trees
          val ${FreshNames.parseResult} = ${FreshNames.parser}($args, ${mock.empty})
          arguments.ParseResult($inst, ${FreshNames.parseResult}.remaining)
        }
      """
    }

    def newInstance(info: TypeInformation) = {
      val params = info.values.map { v ⇒
        AssignOrNamedArg(Ident(v.name), q"${FreshNames.parseResult}.args.${v.name}.get")
      }
      q"new ${info.tpe}(..$params)"
    }

    runMacro
  }
}
