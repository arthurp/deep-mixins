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
    h match {
      case q"object $name extends ..$parents { ..$body }" =>
        val p = parents.head
        println((p, p.getClass))
        q"""
           object $name extends ..$parents {
              def hello: ${typeOf[String]} = "hello"
              ..$body
            }
          """
    }

    val outputs = expandees
    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}

class deepmixin extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro deepmixinMacro.impl
}
