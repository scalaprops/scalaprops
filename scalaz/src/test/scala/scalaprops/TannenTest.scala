package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._
import ScalapropsScalaz._

object TannenTest extends Scalaprops {

  val iListDisjunctionTraverse = scalazlaws.traverse.all[({type l[a] = Tannen[IList, \/, Int, a]})#l]
  val maybeDisjunctionTraverse = scalazlaws.traverse.all[({type l[a] = Tannen[Maybe, \/, Int, a]})#l]
  val iListTuple2Traverse = scalazlaws.traverse.all[({type l[a] = Tannen[IList, Tuple2, Int, a]})#l]
  val maybeTuple2Traverse = scalazlaws.traverse.all[({type l[a] = Tannen[Maybe, Tuple2, Int, a]})#l]

  val iListDisjunctionBitraverse = scalazlaws.bitraverse.all[({type l[a, b] = Tannen[IList, \/, a, b]})#l]
  val maybeDisjunctionBitraverse = scalazlaws.bitraverse.all[({type l[a, b] = Tannen[Maybe, \/, a, b]})#l]
  val iListTuple2Bitraverse = scalazlaws.bitraverse.all[({type l[a, b] = Tannen[IList, Tuple2, a, b]})#l]
  val maybeTuple2Bitraverse = scalazlaws.bitraverse.all[({type l[a, b] = Tannen[Maybe, Tuple2, a, b]})#l]

}
