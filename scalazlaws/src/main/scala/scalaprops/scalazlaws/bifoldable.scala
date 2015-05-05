package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.tuple._

object bifoldable {
  def leftFMConsistent[F[_, _], A, B](implicit F: Bifoldable[F], afa: Gen[F[A, B]], ea: Equal[A], eb: Equal[B]) =
    forAll(F.bifoldableLaw.leftFMConsistent[A, B] _)

  def rightFMConsistent[F[_, _], A, B](implicit F: Bifoldable[F], afa: Gen[F[A, B]], ea: Equal[A], eb: Equal[B]) =
    forAll(F.bifoldableLaw.rightFMConsistent[A, B] _)

  def laws[F[_, _]](implicit fa: Gen[F[Int, Int]], F: Bifoldable[F]) =
    Properties.properties(ScalazLaw.bifoldable) (
      ScalazLaw.bifoldableLeftFMConsistent -> leftFMConsistent[F, Int, Int],
      ScalazLaw.bifoldableRightFMConsistent -> rightFMConsistent[F, Int, Int]
    )

  def all[F[_, _]](implicit fa: Gen[F[Int, Int]], F: Bifoldable[F]) =
    Properties.fromProps(
      ScalazLaw.bifoldableAll -> Maybe.empty[*^*->*],
      bifoldable.laws[F].mapId((_, Maybe.empty[*^*->*])),
      scalazlaws.foldable.laws[({type l[a] = F[a, Int]})#l](implicitly, F.leftFoldable[Int]).mapId((_, Maybe.just(*^*->*.left))),
      scalazlaws.foldable.laws[({type l[a] = F[Int, a]})#l](implicitly, F.rightFoldable[Int]).mapId((_, Maybe.just(*^*->*.right)))
    )
}
