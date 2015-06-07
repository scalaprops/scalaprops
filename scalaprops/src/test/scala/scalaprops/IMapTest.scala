package scalaprops

import scalaz._
import scalaz.std.anyVal._

object IMapTest extends Scalaprops {

  val testLaws =
    Properties.list(
      scalazlaws.bind.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.align.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.zip.all[({type l[a] = Int ==>> a})#l],
      scalazlaws.traverse.all[({type l[a] = Int ==>> a})#l]
    )

  val bifoldable = scalazlaws.bifoldable.all[==>>]
  val order = scalazlaws.order.all[Int ==>> Int]
  val monoid = scalazlaws.monoid.all[Int ==>> Int]

  val conjunction = {
    implicit def imapConjunctionGen[A: Gen: Order, B: Gen]: Gen[((A ==>> B) @@ Tags.Conjunction)] =
      Tag.subst(Gen[A ==>> B])

    implicit val s = IMap.mapIntersection[Int, Int] // poor type inference...
    implicit val e = Tags.Conjunction.subst(Equal[Int ==>> Int])

    scalazlaws.semigroup.all[(Int ==>> Int) @@ Tags.Conjunction]
  }

}
