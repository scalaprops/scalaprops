package scalaprops

import scala.language.experimental.macros
import MacroUtil._

object Macros {

  def assert(assertion: Boolean): Boolean =
    macro Impl.assert1

  def assert(assertion: Boolean, message: Any): Boolean =
    macro Impl.assert2

  /**
   * use `Any#==`
   */
  def assertEqual[A](a: A, b: A): Boolean =
    macro Impl.assertEqual[A]

  /**
   * use `Any#==`
   */
  def assertNotEqual[A](a: A, b: A): Boolean =
    macro Impl.assertNotEqual[A]

  /**
   * use `scalaz.Equal`
   */
  def assertEq[A](a: A, b: A): Boolean =
    macro Impl.assertEq[A]

  /**
   * use `scalaz.Equal`
   */
  def assertNotEq[A](a: A, b: A): Boolean =
    macro Impl.assertNotEq[A]

  private object Impl {

    def assert1(c: Context)(assertion: c.Expr[Boolean]): c.Expr[Boolean] = {
      import c.universe._
      val code = sourceCode(c)(assertion.tree)
      c.Expr(q"""
        if(!$assertion){
          throw new java.lang.AssertionError("assertion failed " + $code)
          false
        }else true
      """)
    }

    def assert2(c: Context)(assertion: c.Expr[Boolean], message: c.Expr[Any]): c.Expr[Boolean] = {
      import c.universe._
      val code = sourceCode(c)(assertion.tree)
      c.Expr(q"""
        if(!$assertion){
          throw new java.lang.AssertionError("assertion failed " + $message + " " + $code)
          false
        }else true
      """)
    }

    def assertEqual[A](c: Context)(a: c.Expr[A], b: c.Expr[A]): c.Expr[Boolean] = {
      import c.universe._
      val codeA = sourceCode(c)(a.tree)
      val codeB = sourceCode(c)(b.tree)
      val aa = freshTermName(c)("a")
      val bb = freshTermName(c)("b")
      val message = freshTermName(c)("message")
      val tree = q"""
        val $aa = $a
        val $bb = $b
        if($aa != $bb){
          val $message = $aa + " [" + $codeA + "] is not equals " + $bb + " [" + $codeB + "]"
          throw new java.lang.AssertionError($message)
          false
        }else true
      """
      c.Expr[Boolean](tree)
    }

    def assertNotEqual[A](c: Context)(a: c.Expr[A], b: c.Expr[A]): c.Expr[Boolean] = {
      import c.universe._
      val codeA = sourceCode(c)(a.tree)
      val codeB = sourceCode(c)(b.tree)
      val aa = freshTermName(c)("a")
      val bb = freshTermName(c)("b")
      val message = freshTermName(c)("message")
      val tree = q"""
        val $aa = $a
        val $bb = $b
        if($aa == $bb){
          val $message = $aa + " [" + $codeA + "] is equals " + $bb + " [" + $codeB + "]"
          throw new java.lang.AssertionError($message)
          false
        }else true
      """
      c.Expr[Boolean](tree)
    }


    def assertEq[A](c: Context)(a: c.Expr[A], b: c.Expr[A])(implicit A: c.WeakTypeTag[A]): c.Expr[Boolean] = {
      import c.universe._
      val codeA = sourceCode(c)(a.tree)
      val codeB = sourceCode(c)(b.tree)
      val aa = freshTermName(c)("a")
      val bb = freshTermName(c)("b")
      val message = freshTermName(c)("message")
      val tree = q"""
        val $aa = $a
        val $bb = $b
        import scalaz.std.AllInstances._
        if(scalaz.Equal[$A].equal($aa, $bb) == false){
          val $message = $aa + " [" + $codeA + "] is not equals " + $bb + " [" + $codeB + "]"
          throw new java.lang.AssertionError($message)
          false
        }else true
      """
      c.Expr[Boolean](tree)
    }

    def assertNotEq[A](c: Context)(a: c.Expr[A], b: c.Expr[A])(implicit A: c.WeakTypeTag[A]): c.Expr[Boolean] = {
      import c.universe._
      val codeA = sourceCode(c)(a.tree)
      val codeB = sourceCode(c)(b.tree)
      val aa = freshTermName(c)("a")
      val bb = freshTermName(c)("b")
      val message = freshTermName(c)("message")
      val tree = q"""
        val $aa = $a
        val $bb = $b
        import scalaz.std.AllInstances._
        if(scalaz.Equal[$A].equal($aa, $bb)){
          val $message = $aa + " [" + $codeA + "] is equals " + $bb + " [" + $codeB + "]"
          throw new java.lang.AssertionError($message)
          false
        }else true
      """
      c.Expr[Boolean](tree)
    }


  }
}
