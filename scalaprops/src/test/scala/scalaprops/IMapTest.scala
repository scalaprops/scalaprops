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

  val mapKeys = {
    type KEY = Short
    type VALUE = Byte

    forAll { (a: KEY ==>> VALUE, f: KEY => KEY) =>
      a.mapKeys(f) == ==>>.fromList(a.toList.map(x => (f(x._1), x._2)))
    }
  }

  val insertWithKey = {
    type KEY = Short
    type VALUE = Byte

    Property.forAll{ (a: KEY ==>> VALUE, k: KEY, v: VALUE, f: (KEY, VALUE, VALUE) => VALUE) =>
      val m = a.toList.toMap
      val i = if(m contains k) {
        k -> f(k, v, m(k))
      } else {
        k -> v
      }
      val x = a.insertWithKey(f, k, v)
      val y = ==>>.fromList((m + i).toList)
      Equal[KEY ==>> VALUE].equal(x, y)
    }.toProperties((), Param.minSuccessful(2000))
  }

  val insertWith = {
    import scalaz.syntax.std.function2._
    type KEY = Short
    type VALUE = Byte

    Property.forAll{ (a: KEY ==>> VALUE, k: KEY, v: VALUE, f: (VALUE, VALUE) => VALUE) =>
      val m = a.toList.toMap
      val x = a.insertWith(f.flip, k, v)
      val y = ==>>.fromList(scalaz.std.map.insertWith(m, k, v)(f).toList)
      Equal[KEY ==>> VALUE].equal(x, y)
    }.toProperties((), Param.minSuccessful(2000))
  }

}
