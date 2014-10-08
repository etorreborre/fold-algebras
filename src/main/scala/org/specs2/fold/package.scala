package org.specs2

import org.specs2.reflect.Compat210.blackbox._

/**
 * Macros to create Names from literals
 */
package object fold {
  import name._

  implicit def Name(string: String): Name =
    macro NameMacro

  def NameMacro(c: Context)(string: c.Expr[String]): c.Expr[Name] = {
    import c.universe._
    string match {
      case Expr(Literal(Constant(v: String))) => nameFromString(c)(v)
      case _ => c.abort(c.enclosingPosition, s"Not a literal ${showRaw(string)}")
    }
  }

  def nameFromString(c: Context)(s: String): c.Expr[Name] = {
    import c.universe._
    name.Name.fromString(s) match {
      case None     => c.abort(c.enclosingPosition,
                               s"$s is not a valid Name. It must not contain a /")
      case Some(fn) => c.Expr(q"Name.unsafe(${fn.name})")
    }
  }

}
