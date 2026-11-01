package scalaprops

import ScalapropsScalaz.*
import scalaz.*
import scalaz.std.anyVal.*
import scalaz.std.tuple.*

object TannenTest extends Scalaprops {
  property("iListDisjunctionTraverse") = scalazlaws.traverse.all[({ type l[a] = Tannen[IList, \/, Int, a] })#l]
  property("maybeDisjunctionTraverse") = scalazlaws.traverse.all[({ type l[a] = Tannen[Maybe, \/, Int, a] })#l]
  property("iListTuple2Traverse") = scalazlaws.traverse.all[({ type l[a] = Tannen[IList, Tuple2, Int, a] })#l]
  property("maybeTuple2Traverse") = scalazlaws.traverse.all[({ type l[a] = Tannen[Maybe, Tuple2, Int, a] })#l]

  property("iListDisjunctionBitraverse") = scalazlaws.bitraverse.all[({ type l[a, b] = Tannen[IList, \/, a, b] })#l]
  property("maybeDisjunctionBitraverse") = scalazlaws.bitraverse.all[({ type l[a, b] = Tannen[Maybe, \/, a, b] })#l]
  property("iListTuple2Bitraverse") = scalazlaws.bitraverse.all[({ type l[a, b] = Tannen[IList, Tuple2, a, b] })#l]
  property("maybeTuple2Bitraverse") = scalazlaws.bitraverse.all[({ type l[a, b] = Tannen[Maybe, Tuple2, a, b] })#l]
}
