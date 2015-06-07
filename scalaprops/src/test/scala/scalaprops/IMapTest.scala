package scalaprops

import scalaz._
import scalaz.std.anyVal._
import Property.forAll

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

  val intersectionWithKey = forAll { (a: Int ==>> Int, b: Int ==>> Int, f: (Int, Int, Int) => Int) =>
    val aa = a.toList.toMap
    val bb = b.toList.toMap
    a.intersectionWithKey(b)(f).toList == scalaz.std.map.intersectWithKey(aa, bb)(f).toList.sorted
  }

}
