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
import scala.util.{Try ⇒ UTry}


class ArgumentsMacro(val c: blackbox.Context) {
  import c.universe._

  object Argument {
    val find: PartialFunction[MethodSymbol, Argument] = {
      case BoolArgument(a) ⇒ a
      case SimpleArgument(a) ⇒ a
    }
  }
  abstract class Argument(val param: TermSymbol) {
    final def name = param.name.decodedName.toString
    final def usage(builder: TermName, default: Option[c.Tree]): c.Tree = {
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
    def tree(parser: TermName): c.Tree
    def typeDef(parser: TermName): c.Tree
  }

  object BoolArgument {
    def unapply(m: MethodSymbol): Option[Argument] =
      if (m.returnType =:= typeOf[Boolean]) Some(new BoolArgument(m)) else None
  }
  final class BoolArgument(param: TermSymbol) extends Argument(param) {
    def tree(parser: TermName): c.Tree = {
      val action = q"""((c) => c.copy(${param.name} = Some(true)))"""
      q"""
        $parser.bool(${param.name.decodedName.toString}, $action)
        """
    }
    def typeDef(builder: TermName) = q""
  }

  object SimpleArgument {
    def unapply(m: MethodSymbol): Option[Argument] =
      Some(new SimpleArgument(m))
  }
  final class SimpleArgument(param: MethodSymbol) extends Argument(param) {
    def tree(parser: TermName): c.Tree = {
      val tpe = param.returnType
      val action = q"""((v, c) => c.copy(${param.name} = Some(v)))"""
      q"""
        $parser.simple[$tpe](${param.name.decodedName.toString}, $action)
        """
    }
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
    lazy val builder = c.freshName(TermName("builder"))
    lazy val parser = c.freshName(TermName("parser"))
    lazy val parseResult = c.freshName(TermName("parseResult"))
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

  object MirrorClass {
    def apply(info: TypeInformation): MirrorClass = {
      val optTpes = optParams(info.tpe, info.accessors)
      val defaultValues = collectDefaults(info)
      mirrorType(info.tpe, optTpes, defaultValues)
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

    private def mirrorType(tpe: Type, optTpes: List[(TermName, Type)], defaults: Map[String, c.Tree]) = {
      val tpeName = c.freshName(tpe.typeSymbol.name.toTypeName)
      val defs = defaults.mapValues(t ⇒ q"Some($t)").withDefault(_ ⇒ q"None")
      val ccTps = optTpes map {
        case (name, t) ⇒ q"val $name: $t = ${defs(name.decodedName.toString)}"
      }
      new MirrorClass(tpeName, defaults, ccTps)
    }
  }
  class MirrorClass(val name: TypeName, val defaults: Map[String, c.Tree], params: List[c.Tree]) {
    val tpe: c.Tree = tq"$name"
    val cls: c.Tree = q"case class $name(..$params)"
    val empty: c.Tree = q"new $name()"
  }

  object ParseTrees {
    def apply(info: TypeInformation): ParseTrees = {
      val arguments = info.accessors collect Argument.find
      new ParseTrees(arguments)
    }
  }
  class ParseTrees(arguments: List[Argument]) {
    def trees(parser: TermName): List[c.Tree] = arguments.map(_.tree(parser))
  }

  object Utils {
    def fail(msg: String): Nothing =
      c.abort(c.enclosingPosition, "\n" + msg)

    def printTree(
      t: c.Tree, label: String = "", printTypes: Boolean = false,
      printIds: Boolean = false, printOwners: Boolean = false,
      printPositions: Boolean = false, printRootPkg: Boolean = false): Unit = {
      c.info(c.enclosingPosition, label + showCode(t, printTypes, printIds, printOwners, printPositions, printRootPkg), force = false)
    }

    def preCheck[A: c.WeakTypeTag]: Type = {
      val tpe = weakTypeOf[A]
      if (!tpe.typeSymbol.isClass) {
        Utils.fail(tpe + "is not a class")
      }
      if (tpe.typeSymbol.companion == NoSymbol) {
        Utils.fail(tpe + "is not a case class or is defined locally")
      }
      tpe
    }
  }

  abstract class MacroDef[A: c.WeakTypeTag, B](label: String, printGenerated: Boolean) {
    protected def code(info: TypeInformation): c.Tree

    final def run: c.Expr[B] = {
      val tpe = Utils.preCheck[A]
      val tpeInfo = TypeInformation(tpe)
      val generated = code(tpeInfo)
      if (printGenerated) {
        Utils.printTree(generated, "Generated " + label + " code: \n\n")
      }
      c.Expr[B](generated)
    }
  }

  final class ParserDef[A: c.WeakTypeTag](providerTpe: Type, args: c.Expr[Array[String]], printGenerated: Boolean) extends MacroDef[A, UTry[ParseResult[A]]]("parser", printGenerated) {
    protected def code(info: TypeInformation): c.Tree = {
      val mirror = MirrorClass(info)
      val trees = ParseTrees(info).trees(FreshNames.parser)
      val inst = newInstance(info)

      q"""
        {
          ${mirror.cls}
          val ${FreshNames.parser} = implicitly[$providerTpe].apply[${mirror.tpe}]
          ..$trees
          scala.util.Try {
            val ${FreshNames.parseResult} = ${FreshNames.parser}($args, ${mirror.empty})
            arguments.ParseResult($inst, ${FreshNames.parseResult}.remaining)
          }
        }
      """
    }

    private def newInstance(info: TypeInformation) = {
      val params = info.values.map { v ⇒
        val exMessage = v.name.decodedName.toString + " is missing"
        AssignOrNamedArg(Ident(v.name),
          q"""${FreshNames.parseResult}.args.${v.name}.getOrElse(throw new IllegalArgumentException($exMessage))""")
      }
      q"new ${info.tpe}(..$params)"
    }
  }

  final class UsageDef[A: c.WeakTypeTag](printGenerated: Boolean) extends MacroDef[A, String]("usage", printGenerated) {
    protected def code(info: TypeInformation): c.Tree = {
      val mirror = MirrorClass(info)
      val defaults = mirror.defaults
      val arguments = info.accessors collect Argument.find
      val trees = arguments.map(a ⇒ a.usage(FreshNames.builder, defaults.get(a.name)))

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
  }

  def parse[A: c.WeakTypeTag](args: c.Expr[Array[String]]): c.Expr[UTry[ParseResult[A]]] = {
    new ParserDef[A](weakTypeOf[ParserProvider], args, false).run
  }

  def usage[A: c.WeakTypeTag]: c.Expr[String] = {
    new UsageDef[A](false).run
  }

  def parseDebug[A: c.WeakTypeTag](args: c.Expr[Array[String]]): c.Expr[UTry[ParseResult[A]]] = {
    new ParserDef[A](weakTypeOf[ParserProvider], args, true).run
  }

  def usageDebug[A: c.WeakTypeTag]: c.Expr[String] = {
    new UsageDef[A](true).run
  }
}
