package scalaprops

import Property.forAll
import ScalapropsScalaz.*
import scalaz.*
import scalaz.Id.Id
import scalaz.Maybe.Just
import scalaz.std.anyVal.*
import scalaz.syntax.equal.*

@scalajs.js.annotation.JSExportAll
object IMapTest extends Scalaprops {
  val testLaws =
    Properties.list(
      scalazlaws.bind.all[({ type l[a] = Int ==>> a })#l],
      scalazlaws.align.all[({ type l[a] = Int ==>> a })#l],
      scalazlaws.zip.all[({ type l[a] = Int ==>> a })#l],
      scalazlaws.traverse.all[({ type l[a] = Int ==>> a })#l]
    )

  val bifoldable = scalazlaws.bifoldable.all[==>>]
  val order = scalazlaws.order.all[Int ==>> Int]
  val laws2 = Properties.list(
    scalazlaws.monoid.all[Int ==>> Int],
    scalazlaws.semilattice.all[Int ==>> ISet[Int]]
  )

  val conjunction = {
    implicit def imapConjunctionGen[A: Gen: Order, B: Gen]: Gen[((A ==>> B) @@ Tags.Conjunction)] =
      Tag.subst(Gen[A ==>> B])

    implicit val s: Semigroup[(Int ==>> Int) @@ Tags.Conjunction] = IMap.mapIntersection[Int, Int]
    implicit val e: Equal[(Int ==>> Int) @@ Tags.Conjunction] = Tags.Conjunction.subst(Equal[Int ==>> Int])

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

    forAll { (a: KEY ==>> VALUE, f: KEY => KEY) => a.mapKeys(f) == ==>>.fromList(a.toList.map(x => (f(x._1), x._2))) }
  }

  val insertWithKey = {
    type KEY = Short
    type VALUE = Byte

    Property.forAll { (a: KEY ==>> VALUE, k: KEY, v: VALUE, f: (KEY, VALUE, VALUE) => VALUE) =>
      val m = a.toList.toMap
      val i = if (m contains k) {
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
    import scalaz.syntax.std.function2.*
    type KEY = Short
    type VALUE = Byte

    Property.forAll { (a: KEY ==>> VALUE, k: KEY, v: VALUE, f: (VALUE, VALUE) => VALUE) =>
      val m = a.toList.toMap
      val x = a.insertWith(f.flip, k, v)
      val y = ==>>.fromList(scalaz.std.map.insertWith(m, k, v)(f).toList)
      Equal[KEY ==>> VALUE].equal(x, y)
    }.toProperties((), Param.minSuccessful(2000))
  }

  val updateWithKey = {
    type KEY = Byte
    type VAL = Byte
    val E = Equal[KEY ==>> VAL]

    Property.forAll { (a: KEY ==>> VAL, k: KEY, f: (KEY, VAL) => Maybe[VAL]) =>
      val r = a.updateWithKey(k, f)
      a.lookup(k) match {
        case Just(v1) =>
          f(k, v1) match {
            case Just(v2) =>
              E.equal(a.delete(k).insert(k, v2), r)
            case Maybe.Empty() =>
              E.equal(a.delete(k), r)
          }
        case Maybe.Empty() =>
          E.equal(a, r)
      }
    }.toProperties((), Param.minSuccessful(5000))
  }

  val updateLookupWithKey = {
    type KEY = Byte
    type VAL = Byte
    val E = Equal[KEY ==>> VAL]

    Property.forAll { (a: KEY ==>> VAL, k: KEY, f: (KEY, VAL) => Maybe[VAL]) =>
      val (o, r) = a.updateLookupWithKey(k, f)
      assert(E.equal(r, a.updateWithKey(k, f)))

      a.lookup(k) match {
        case Just(v1) =>
          f(k, v1) match {
            case Just(v2) =>
              E.equal(a.delete(k).insert(k, v2), r) && (Maybe.just(v2) === o)
            case Maybe.Empty() =>
              E.equal(a.delete(k), r) && (Maybe.just(v1) === o)
          }
        case Maybe.Empty() =>
          E.equal(a, r) && o.isEmpty
      }
    }.toProperties((), Param.minSuccessful(5000))
  }

  val alter = {
    type KEY = Byte
    type VAL = Byte
    val E = Equal[KEY ==>> VAL]

    Property.forAll { (a: KEY ==>> VAL, k: KEY, f: Maybe[VAL] => Maybe[VAL]) =>
      val r = a.alter(k, f)
      a.lookup(k) match {
        case Just(v1) =>
          f(Maybe.just(v1)) match {
            case Just(v2) =>
              E.equal(a.insert(k, v2), r)
            case Maybe.Empty() =>
              E.equal(a.delete(k), r)
          }
        case Maybe.Empty() =>
          f(Maybe.empty) match {
            case Just(v2) =>
              E.equal(a.insert(k, v2), r)
            case Maybe.Empty() =>
              E.equal(a, r)
          }
      }
    }.toProperties((), Param.minSuccessful(5000))
  }

  val updateAt = {
    type KEY = Byte
    type VAL = Byte
    val E = Equal[KEY ==>> VAL]

    Property.NoShrink.property2 { (a0: NonEmptyList[(KEY, VAL)], f: (KEY, VAL) => Maybe[VAL]) =>
      val a = IMap.fromFoldable(a0)
      Property.forAllG(Gen.choose(0, a.size - 1)) { i =>
        val r = a.updateAt(i, f)
        a.elemAt(i) match {
          case Just((k, v1)) =>
            f(k, v1) match {
              case Just(v2) =>
                E.equal(r, a.update(k, _ => Maybe.just(v2)))
              case Maybe.Empty() =>
                E.equal(a.deleteAt(i), r) && E.equal(a.delete(k), r)
            }
          case Maybe.Empty() =>
            E.equal(a, r)
        }
      }
    }.toProperties((), Param.minSuccessful(5000))
  }

  val updateMinWithKey = {
    type KEY = Byte
    type VAL = Byte
    val E = Equal[KEY ==>> VAL]

    Property.forAll { (a: (KEY ==>> VAL), f: (KEY, VAL) => Maybe[VAL]) =>
      val b = a.updateMinWithKey(f)
      a.minViewWithKey match {
        case Just(((k, v1), c)) =>
          f(k, v1) match {
            case Just(v2) =>
              (a.size == b.size) && E.equal(b, c.insert(k, v2))
            case Maybe.Empty() =>
              ((a.size - 1) == b.size) && E.equal(b, c)
          }
        case Maybe.Empty() =>
          a.isEmpty && b.isEmpty
      }
    }.toProperties((), Param.minSuccessful(5000))
  }

  val updateMaxWithKey = {
    type KEY = Byte
    type VAL = Byte
    val E = Equal[KEY ==>> VAL]

    Property.forAll { (a: (KEY ==>> VAL), f: (KEY, VAL) => Maybe[VAL]) =>
      val b = a.updateMaxWithKey(f)
      a.maxViewWithKey match {
        case Just(((k, v1), c)) =>
          f(k, v1) match {
            case Just(v2) =>
              (a.size == b.size) && E.equal(b, c.insert(k, v2))
            case Maybe.Empty() =>
              ((a.size - 1) == b.size) && E.equal(b, c)
          }
        case Maybe.Empty() =>
          a.isEmpty && b.isEmpty
      }
    }.toProperties((), Param.minSuccessful(5000))
  }

  val unionWithKey = {
    type KEY = Byte
    type VAL = Byte
    val E = Equal[KEY ==>> VAL]

    Property.forAll { (a: KEY ==>> VAL, b: KEY ==>> VAL, f: (KEY, VAL, VAL) => VAL) =>
      val c = a.unionWithKey(b)(f)
      val aa = a.toList.toMap
      val bb = b.toList.toMap
      val cc = scalaz.std.map.unionWithKey(aa, bb)(f)
      E.equal(IMap.fromList(cc.toList), c)
    }.toProperties((), Param.minSuccessful(5000))
  }

  val traverseWithKey = {
    type KEY = Byte
    type VAL = Int
    type C = Short
    val T = Traverse[({ type l[a] = KEY ==>> a })#l]

    def test[F[_]: Applicative](implicit E: Equal[F[KEY ==>> C]], G: Gen[F[C]]) =
      Property.forAll { (a: KEY ==>> VAL, f: VAL => F[C]) =>
        val g: (KEY, VAL) => F[C] = (_, v) => f(v)
        val x = T.traverse(a)(f)
        val y = a.traverseWithKey(g)
        E.equal(x, y)
      }

    Properties
      .list(
        test[Id].toProperties("Id"),
        test[Maybe].toProperties("Maybe"),
        test[IList].toProperties("IList", Param.maxSize(5))
      )
      .andThenParam(Param.minSuccessful(5000))
  }

  val foldMapWithKey = {
    type KEY = Byte
    type VAL = Int
    type C = IList[Byte]
    val F = Foldable[({ type l[a] = KEY ==>> a })#l]

    Property.forAll { (a: KEY ==>> VAL, f: VAL => C) =>
      val g: (KEY, VAL) => C = (_, v) => f(v)
      val x = F.foldMap(a)(f)
      val y = a.foldMapWithKey(g)
      val z = Foldable[IList].foldMap(a.values)(f)
      Equal[C].equal(x, y) && Equal[C].equal(y, z)
    }.toProperties((), Param.minSuccessful(5000) andThen Param.maxSize(3))
  }
}
