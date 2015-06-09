package scalaprops

final case class CogenState[A](rand: Rand, gen: Gen[A])
