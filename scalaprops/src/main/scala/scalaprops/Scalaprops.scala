package scalaprops

import scalaz._

trait Scalaprops {

  def param: Param = Param.withCurrentTimeSeed()

  def listener: ScalapropsListener =
    ScalapropsListener.default

  def transformProperties[A](properties: List[Properties[A]]): List[Properties[A]] =
    properties.map(Scalaprops.filterUnitEmpty).sortBy(_.id.toString)

}

object Scalaprops {

  def filterUnitEmpty[A](p: Properties[A]): Properties[A] = {
    def loop(tree: Tree[(A, Maybe[Check])]): Tree[(A, Maybe[Check])] =
      tree match {
        case Tree.Node(root, Stream(Tree.Node((Or.L(()), Maybe.Empty()), sub))) =>
          Tree.node(root, sub.map(loop))
        case _ =>
          Tree.node(tree.rootLabel, tree.subForest.map(loop))
      }
    Properties.noSort(loop(p.props))
  }

}
