package scalaprops

/** Gen and Shrink pair */
final case class GS[A](implicit val gen: Gen[A], implicit val shrink: Shrink[A])

object GS {
  implicit def wrap[A](implicit G: Gen[A], S: Shrink[A]): GS[A] =
    GS()(G, S)
}
