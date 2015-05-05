package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object monadError {

  def raisedErrorsHandled[F[_, _], E, A](implicit me: MonadError[F, E], eq: Equal[F[E, A]], ae: Gen[E], afea: Gen[E => F[E,A]]) =
    forAll(me.monadErrorLaw.raisedErrorsHandled[A] _)

  def errorsRaised[F[_, _], E, A](implicit me: MonadError[F, E], eq: Equal[F[E, A]], ae: Gen[E], aa: Gen[A]) =
    forAll(me.monadErrorLaw.errorsRaised[A] _)

  def errorsStopComputation[F[_, _], E, A](implicit me: MonadError[F, E], eq: Equal[F[E, A]], ae: Gen[E], aa: Gen[A]) =
    forAll(me.monadErrorLaw.errorsStopComputation[A] _)

  def laws[F[_, _], E: Gen](implicit A1: MonadError[F, E], A2: Gen[F[E, Int]], A3: Gen[F[E, Int => Int]], A4: Equal[F[E, Int]], A5: Gen[E => F[E, Int]]) =
    Properties.properties(ScalazLaw.monadError) (
      ScalazLaw.monadErrorRaisedErrorsHandled -> raisedErrorsHandled[F, E, Int],
      ScalazLaw.monadErrorErrorsRaised -> errorsRaised[F, E, Int],
      ScalazLaw.monadErrorErrorsStopComputation ->  errorsStopComputation[F, E, Int]
    )

  def all[F[_, _], E: Gen](implicit A1: MonadError[F, E], A2: Gen[F[E, Int]], A3: Gen[F[E, Int => Int]], A4: Equal[F[E, Int]], A5: Gen[E => F[E, Int]]) =
    Properties.fromProps(ScalazLaw.monadErrorAll, monadError.laws[F, E], monad.all[({type l[a] = F[E, a]})#l])
}
