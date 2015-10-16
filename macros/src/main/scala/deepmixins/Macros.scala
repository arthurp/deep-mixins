package deepmixins

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation

object deepmixinMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val inputs = annottees.map(_.tree).toList
    val (annottee, expandees) = inputs match {
      case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
      case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
      case _ => (EmptyTree, inputs)
    }
    println((annottee, expandees))
    val h = expandees.head
    val res = h match {
      case q"object $name extends ..$baseIds { ..$body }" =>
        val bases = baseIds.map { id =>
          val typed = c.typecheck(id.duplicate, mode = c.TYPEmode)
          typed.symbol.asType.toType
        }
        val clsMems = bases.map { b => (b, b.members.filter(_.isClass).map(_.asClass)) }
        val nestedMixes = for (m <- clsMems.head._2) yield {
          val matchingMems = clsMems.collect {
            case (b, ms) if ms.exists(_.name == m.name) => (b, ms.find(_.name == m.name).get)
          }
          val superClss = matchingMems.map {
            case (base, mem) =>
              tq"super[${base.typeSymbol.asClass.name}].${mem.name}"
          }
          q"""class ${m.name} extends ..${superClss}"""
        }
        q"""
           object $name extends ..$bases {
              ..$body
              ..$nestedMixes
            }
          """
    }
    println(res)

    val outputs = res :: expandees.tail
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}

class deepmixin extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro deepmixinMacro.impl
}
