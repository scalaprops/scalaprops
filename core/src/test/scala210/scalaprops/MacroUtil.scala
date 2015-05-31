package scalaprops

object MacroUtil {

  type Context = scala.reflect.macros.Context

  def freshTermName(c: Context)(s: String) =
    c.universe.newTermName(c.fresh(s))

  def termName(c: Context)(s: String) =
    c.universe.newTermName(s)

  def sourceCode(c: Context)(t: c.Tree) =
    c.universe.show(t)
}
