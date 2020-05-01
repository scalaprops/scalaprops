package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object monadError {
  def raisedErrorsHandled[F[_], E, A](implicit
    me: MonadError[F, E],
    eq: Equal[F[A]],
    ae: Gen[E],
    afea: Gen[E => F[A]]
  ) =
    forAll(me.monadErrorLaw.raisedErrorsHandled[A] _)

  def errorsRaised[F[_], E, A](implicit me: MonadError[F, E], eq: Equal[F[A]], ae: Gen[E], aa: Gen[A]) =
    forAll(me.monadErrorLaw.errorsRaised[A] _)

  def errorsStopComputation[F[_], E, A](implicit me: MonadError[F, E], eq: Equal[F[A]], ae: Gen[E], aa: Gen[A]) =
    forAll(me.monadErrorLaw.errorsStopComputation[A] _)

  def laws[F[_], E: Gen](implicit A1: MonadError[F, E], A2: Equal[F[Int]], A3: Gen[E => F[Int]]) =
    Properties.properties(ScalazLaw.monadError)(
      ScalazLaw.monadErrorRaisedErrorsHandled -> raisedErrorsHandled[F, E, Int],
      ScalazLaw.monadErrorErrorsRaised -> errorsRaised[F, E, Int],
      ScalazLaw.monadErrorErrorsStopComputation -> errorsStopComputation[F, E, Int]
    )

  def all[F[_], E: Gen](implicit
    A1: MonadError[F, E],
    A2: Gen[F[Int]],
    A3: Gen[F[Int => Int]],
    A4: Equal[F[Int]],
    A5: Gen[E => F[Int]]
  ) =
    Properties.fromProps(ScalazLaw.monadErrorAll, monadError.laws[F, E], monad.all[F])
}
