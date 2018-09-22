package scalaprops

final case class CogenState[A](rand: Rand, gen: Gen[A]) {
  def map[B](f: A => B): CogenState[B] =
    CogenState(rand, gen.map(f))
}
