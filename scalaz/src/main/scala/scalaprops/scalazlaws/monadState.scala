package scalaprops
package scalazlaws

import scalaz._
import Property.forAll

object monadState {
  def putPut[F[_], S](implicit F: MonadState[F, S], S: Gen[S], E: Equal[F[Unit]]): Property =
    forAll(MonadStateLaws[F, S].putPut _)

  def putGet[F[_], S](implicit F: MonadState[F, S], S: Gen[S], E: Equal[F[S]]): Property =
    forAll(MonadStateLaws[F, S].putGet _)

  def getPut[F[_], S](implicit F: MonadState[F, S], E: Equal[F[Unit]]): Property =
    forAll(MonadStateLaws[F, S].getPut)

  def getGet[F[_], S](implicit F: MonadState[F, S], K: Gen[(S, S) => F[Unit]], E: Equal[F[Unit]]): Property =
    forAll(MonadStateLaws[F, S].getGet _)

  def laws[F[_], S](
    implicit
    F: MonadState[F, S],
    S: Gen[S],
    K: Gen[(S, S) => F[Unit]],
    E1: Equal[F[Unit]],
    E2: Equal[F[S]]
  ) = Properties.properties(ScalazLaw.monadState)(
    ScalazLaw.monadStatePutPut -> putPut[F, S],
    ScalazLaw.monadStatePutGet -> putGet[F, S],
    ScalazLaw.monadStateGetPut -> getPut[F, S],
    ScalazLaw.monadStateGetGet -> getGet[F, S]
  )

  def all[F[_], S](
    implicit
    F: MonadState[F, S],
    S: Gen[S],
    G1: Gen[F[Int]],
    G2: Gen[F[Int => Int]],
    K: Gen[(S, S) => F[Unit]],
    E1: Equal[F[Unit]],
    E2: Equal[F[S]],
    E3: Equal[F[Int]]
  ) = Properties.fromProps(
    ScalazLaw.monadStateAll,
    monad.all[F],
    monadState.laws[F, S]
  )
}
