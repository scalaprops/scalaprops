package scalaprops

final class Neko private(val ord: Int, val fullName: String, val simpleName: String) {
  override def hashCode = ord
  override def toString = simpleName
}

object Neko {
  private[this] val set = collection.mutable.Set.empty[Neko]

  private[this] def law(fullName: String, simpleName: String = ""): Neko =
    set.synchronized{
      val name = if(simpleName == "") fullName else simpleName
      val l = new Neko(set.size, fullName, name)
      set += l
      l
    }

  private[this] def law0(clazz: Neko, lawName: String): Neko =
    law(clazz.simpleName + " " + lawName, lawName)

  private[this] def all(clazz: Neko): Neko =
    law(clazz.simpleName+ " all", clazz.simpleName)

  val invariant = law("invariant")
  val invariantIdentity = law0(invariant, "identity")
  val invariantComposition = law0(invariant, "composition")

  val functor = law("functor")
  val functorAll = all(functor)
  val functorIdentity = law0(functor, "identity")
  val functorComposition = law0(functor, "composite")

  val apply = law("apply")
  val applyAll = all(apply)
  val applyComposition = law0(apply, "composition")

  val applicative = law("applicative")
  val applicativeAll = all(applicative)
  val applicativeIdentity = law0(applicative, "identity")
  val applicativeHomomorphism = law0(applicative, "homomorphism")
  val applicativeInterchange = law0(applicative, "interchange")
  val applicativeMap = law0(applicative, "map")

  val applicativePlus = law("applicativePlus")

  val flatmap = law("flatmap")
  val flatmapAll = all(flatmap)
  val flatmapAssociativity = law0(flatmap, "associativity")
  val flatmapConsistentApply = law0(flatmap, "consistent apply")
  val flatmapKleisliAssociativity = law0(flatmap, "kleisli associativity")

  val monad = law("monad")
  val monadAll = all(monad)
  val monadRightIdentity = law0(monad, "right identity")
  val monadLeftIdentity = law0(monad, "left identity")
  val monadKleisliRightIdentity = law0(monad, "kleisli right identity")
  val monadKleisliLeftIdentity = law0(monad, "kleisli left identity")
  val monadMapFlatMapCoherence = law0(monad, "map flatMap coherence")

  val values: List[Neko] = set.toList

  implicit val nekoGen: Gen[Neko] = {
    val h :: t = values
    Gen.elements(h, t: _*)
  }

  implicit val nekoOrder: scalaz.Order[Neko] = {
    import scalaz.std.anyVal._
    scalaz.Order.orderBy(_.ord)
  }
}
