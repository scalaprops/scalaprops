package scalaprops

import scalaz.Functor

final case class CogenState[A](rand: Rand, gen: Gen[A]) {
  def map[B](f: A => B): CogenState[B] =
    CogenState(rand, gen.map(f))
}

object CogenState {
  implicit val instance: Functor[CogenState] =
    new Functor[CogenState] {
      override def map[A, B](fa: CogenState[A])(f: A => B) =
        fa map f
    }
}
