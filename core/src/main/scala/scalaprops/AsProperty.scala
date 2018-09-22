package scalaprops

/**
 * @see [[scalaprops.Bool]]
 */
abstract class AsProperty[A] { self =>
  def asProperty(a: A): Property
  def toFunction: A => Property = asProperty
  final def contramap[B](f: B => A): AsProperty[B] =
    new AsProperty[B] {
      def asProperty(b: B) = self.asProperty(f(b))
    }
}

object AsProperty {
  def from[A](f: A => Property): AsProperty[A] =
    new AsProperty[A] {
      override val toFunction = f
      def asProperty(a: A) = f(a)
    }

  implicit val bool: AsProperty[Bool] =
    from(b => Property.prop(b.b))

  implicit val boolean: AsProperty[Boolean] =
    from(Property.prop)

  implicit val property: AsProperty[Property] =
    from(identity)
}
