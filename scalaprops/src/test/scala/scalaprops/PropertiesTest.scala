package scalaprops

import scalaprops.Property.forAll
import scalaz._
import scalaz.std.string._

object PropertiesTest extends Scalaprops {

  // http://typelevel.org/blog/2013/11/17/discipline.html

  val ` "directed acyclic graph" to tree` = forAll {

    val functor = Tree.Leaf("functor")
    val cobind = Tree.Node("cobind", Stream(functor))
    val apply = Tree.Node("apply", Stream(functor))
    val applicative = Tree.Node("applicative", Stream(apply))
    val bind = Tree.Node("bind", Stream(apply))
    val monad = Tree.Node("monad", Stream(bind, applicative))
    val foldable = Tree.Leaf("foldable")
    val traverse = Tree.Node("traverse", Stream(foldable, functor))
    val plus = Tree.Leaf("plus")
    val plusEmpty = Tree.Node("plusEmpty", Stream(plus))
    val applicativePlus = Tree.Node("applicativePlus", Stream( applicative, plusEmpty))
    val monadPlus = Tree.Node("monadPlus", Stream(applicativePlus, monad))
    val isEmpty = Tree.Node("isEmpty", Stream(plusEmpty))

    val list = Tree.Node("list instance", Stream(
      isEmpty, monadPlus, traverse, cobind
    ))

    val dag = """"list instance"
|
+- "isEmpty"
|  |
|  `- "plusEmpty"
|     |
|     `- "plus"
|
+- "monadPlus"
|  |
|  +- "applicativePlus"
|  |  |
|  |  +- "applicative"
|  |  |  |
|  |  |  `- "apply"
|  |  |     |
|  |  |     `- "functor"
|  |  |
|  |  `- "plusEmpty"
|  |     |
|  |     `- "plus"
|  |
|  `- "monad"
|     |
|     +- "bind"
|     |  |
|     |  `- "apply"
|     |     |
|     |     `- "functor"
|     |
|     `- "applicative"
|        |
|        `- "apply"
|           |
|           `- "functor"
|
+- "traverse"
|  |
|  +- "foldable"
|  |
|  `- "functor"
|
`- "cobind"
   |
   `- "functor"
"""

    assert(list.drawTree == dag)

    val clazz = Properties.getClass
    val methodName = "distinctTree"
    val method = clazz.getDeclaredMethods.find(_.getName == methodName).getOrElse(sys.error("not found " + methodName))
    method.setAccessible(true)
    val tree = method.invoke(Properties, list, Order[String]).asInstanceOf[Tree[String]]
    tree.drawTree == """"list instance"
|
+- "isEmpty"
|  |
|  `- "plusEmpty"
|     |
|     `- "plus"
|
+- "monadPlus"
|  |
|  +- "applicativePlus"
|  |  |
|  |  `- "applicative"
|  |     |
|  |     `- "apply"
|  |        |
|  |        `- "functor"
|  |
|  `- "monad"
|     |
|     `- "bind"
|
+- "traverse"
|  |
|  `- "foldable"
|
`- "cobind"
"""
  }

}
