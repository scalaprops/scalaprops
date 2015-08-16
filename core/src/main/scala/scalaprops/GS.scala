package scalaprops

/** Gen and Shrink pair */
final case class GS[A] private(private val g: Gen[A], private val s: Shrink[A]) {
  implicit val gen = g
  implicit val shrink = s
}

object GS {
  implicit def wrap[A](implicit G: Gen[A], S: Shrink[A]): GS[A] =
    GS(G, S)

  implicit def toGen[A](implicit GS: GS[A]): Gen[A] =
    GS.g

  implicit def toShrink[A](implicit GS: GS[A]): Shrink[A] =
    GS.s
}
