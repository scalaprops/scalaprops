package scalaprops
package scalazlaws

import scalaz._

private[scalazlaws] object ArrowLaws {
  def apply[=>:[_, _]](implicit F: Arrow[=>:]) = new ArrowLaws[=>:]
}

private[scalazlaws] final class ArrowLaws[=>:[_, _]](implicit F: Arrow[=>:]) {
  import F._
  import F.arrowSyntax._
  import std.function._

  def arrowIdentity[A](implicit E: Equal[A =>: A]): Boolean =
    E.equal(arr(identity), id)

  def arrowComposition[A, B, C](ab: A => B, bc: B => C)(implicit E: Equal[A =>: C]): Boolean =
    E.equal(arr(ab andThen bc), arr(ab) >>> arr(bc))

  def arrowExtension[A, B, C](ab: A => B)(implicit E: Equal[(A, C) =>: (B, C)]): Boolean =
    E.equal(arr(ab).first[C], arr(Arrow[Function1].split(ab, identity)))

  def arrowFunctor[A, B, C, D](ab: A =>: B, bc: B =>: C)(implicit E: Equal[(A, D) =>: (C, D)]): Boolean =
    E.equal((ab >>> bc).first[D], ab.first[D] >>> bc.first[D])

  def arrowExchange[A, B, C, D](f: A =>: B, g: C => D)(implicit E: Equal[(A, C) =>: (B, D)]): Boolean =
    E.equal(
      f.first[C] >>> arr(Split[Function1].split(identity[B], g)),
      arr(Split[Function1].split(identity[A], g)) >>> f.first[D]
    )

  def arrowUnit[A, B, C](f: A =>: B)(implicit E: Equal[(A, C) =>: B]): Boolean =
    E.equal(f.first[C] >>> arr(fst[B, C]), arr(fst[A, C]) >>> f)

  def arrowAssociation[A, B, C, D](f: A =>: B)(implicit E: Equal[((A, C), D) =>: (B, (C, D))]): Boolean =
    E.equal(f.first[C].first[D] >>> F.arr(assoc[B, C, D]), F.arr(assoc[A, C, D]) >>> f.first[(C, D)])

  private[this] def fst[A, B](p: (A, B)): A = p._1

  private[this] def assoc[A, B, C](p: ((A, B), C)): (A, (B, C)) = (p._1._1, (p._1._2, p._2))
}
