package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz.*
import scalaz.std.anyVal.*

object bifoldable {
  def leftFMConsistent[F[_, _], A, B](implicit F: Bifoldable[F], afa: Gen[F[A, B]], ea: Equal[A], eb: Equal[B]) =
    forAll(F.bifoldableLaw.leftFMConsistent[A, B] _)

  def rightFMConsistent[F[_, _], A, B](implicit F: Bifoldable[F], afa: Gen[F[A, B]], ea: Equal[A], eb: Equal[B]) =
    forAll(F.bifoldableLaw.rightFMConsistent[A, B] _)

  def laws[F[_, _]](implicit fa: Gen[F[Int, Int]], F: Bifoldable[F]) =
    Properties.properties(ScalazLaw.bifoldable)(
      ScalazLaw.bifoldableLeftFMConsistent -> leftFMConsistent[F, Int, Int],
      ScalazLaw.bifoldableRightFMConsistent -> rightFMConsistent[F, Int, Int]
    )

  def all[F[_, _]](implicit fa: Gen[F[Int, Int]], F: Bifoldable[F]): Properties[(ScalazLaw, *^*->*.T)] =
    Properties.fromProps[(ScalazLaw, *^*->*.T)](
      ScalazLaw.bifoldableAll -> *^*->*.Empty,
      bifoldable.laws[F].mapId((_, *^*->*.Empty)),
      scalazlaws.foldable.laws[({ type l[a] = F[a, Int] })#l](implicitly, F.leftFoldable[Int]).mapId((_, *^*->*.L)),
      scalazlaws.foldable.laws[({ type l[a] = F[Int, a] })#l](implicitly, F.rightFoldable[Int]).mapId((_, *^*->*.R))
    )
}
