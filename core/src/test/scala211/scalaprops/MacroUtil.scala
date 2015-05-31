package scalaprops

object MacroUtil {

  type Context = scala.reflect.macros.whitebox.Context

  def freshTermName(c: Context)(s: String) =
    c.universe.TermName(c.freshName(s))

  def termName(c: Context)(s: String) =
    c.universe.TermName(s)

  def sourceCode(c: Context)(t: c.Tree) =
    c.universe.showCode(t)
}
