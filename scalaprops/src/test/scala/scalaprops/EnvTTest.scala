package scalaprops

import matryoshka.patterns.EnvT
import scalaz.Id.Id
import scalaz._
import scalaz.std.tuple._
import scalaz.std.anyVal._

object EnvTTest extends Scalaprops {

  private[this] implicit def envTGen[W[_], E, A](implicit G: Gen[(E, W[A])]): Gen[EnvT[E, W, A]] =
    G.map(EnvT(_))

  private[this] implicit def envTCogen[W[_], E, A](implicit C: Cogen[(E, W[A])]): Cogen[EnvT[E, W, A]] =
    C.contramap(_.run)

  private[this] implicit def envTEqual[W[_], E, A](implicit E: Equal[(E, W[A])]): Equal[EnvT[E, W, A]] =
    E.contramap(_.run)

  val id = {
    type F[A] = EnvT[Int, Id, A]
    scalazlaws.comonad.all[F]
  }

  val tree = {
    type F[A] = EnvT[Int, Tree, A]
    scalazlaws.comonad.all[F]
  }

  val nel = {
    type F[A] = EnvT[Int, NonEmptyList, A]
    scalazlaws.comonad.all[F]
  }

}
