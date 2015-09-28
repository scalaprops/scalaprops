package scalaprops
package scalazlaws

import scalaz._
import Property.forAll

object monadState {

  def putPut[F[_, _], S](implicit F: MonadState[F, S], S: Gen[S], E: Equal[F[S, Unit]]): Property =
    forAll(MonadStateLaws[F, S].putPut _)

  def putGet[F[_, _], S](implicit F: MonadState[F, S], S: Gen[S], E: Equal[F[S, S]]): Property =
    forAll(MonadStateLaws[F, S].putGet _)

  def getPut[F[_, _], S](implicit F: MonadState[F, S], E: Equal[F[S, Unit]]): Property =
    forAll(MonadStateLaws[F, S].getPut)

  def getGet[F[_, _], S](implicit F: MonadState[F, S], K: Gen[(S, S) => F[S, Unit]], E: Equal[F[S, Unit]]): Property =
    forAll(MonadStateLaws[F, S].getGet _)

  def laws[F[_, _], S](implicit
    F: MonadState[F, S],
    S: Gen[S],
    K: Gen[(S, S) => F[S, Unit]],
    E1: Equal[F[S, Unit]],
    E2: Equal[F[S, S]]
  ) = Properties.properties(ScalazLaw.monadState)(
    ScalazLaw.monadStatePutPut -> putPut[F, S],
    ScalazLaw.monadStatePutGet -> putGet[F, S],
    ScalazLaw.monadStateGetPut -> getPut[F, S],
    ScalazLaw.monadStateGetGet -> getGet[F, S]
  )

  def all[F[_, _], S](implicit
    F: MonadState[F, S],
    S: Gen[S],
    G1: Gen[F[S, Int]],
    G2: Gen[F[S, Int => Int]],
    K: Gen[(S, S) => F[S, Unit]],
    E1: Equal[F[S, Unit]],
    E2: Equal[F[S, S]],
    E3: Equal[F[S, Int]]
  ) = Properties.fromProps(
    ScalazLaw.monadStateAll, monad.all[({type l[a] = F[S, a]})#l], monadState.laws[F, S]
  )

}
