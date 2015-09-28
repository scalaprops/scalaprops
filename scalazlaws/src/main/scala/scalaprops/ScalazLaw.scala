package scalaprops

import scalaz._

final class ScalazLaw private(val ord: Int, val fullName: String, val simpleName: String) {
  override def hashCode = ord
  override def toString = simpleName
}

object ScalazLaw {
  private[this] val set = collection.mutable.Set.empty[ScalazLaw]

  private[this] def law(fullName: String, simpleName: String = ""): ScalazLaw =
    set.synchronized{
      val name = if(simpleName == "") fullName else simpleName
      val l = new ScalazLaw(set.size, fullName, name)
      set += l
      l
    }

  private[this] def law0(clazz: ScalazLaw, lawName: String): ScalazLaw =
    law(clazz.simpleName + " " + lawName, lawName)

  private[this] def all(clazz: ScalazLaw): ScalazLaw =
    law(clazz.simpleName+ " all", clazz.simpleName)

  val equal = law("equal")
  val equalCommutativity = law0(equal, "commutativity")
  val equalReflexive = law0(equal, "reflexive")
  val equalTransitive = law0(equal, "transitive")
  val equalNaturality = law0(equal, "naturality")

  val order = law("order")
  val orderAll = all(order)
  val orderAntisymmetric = law0(order, "anti symmetric")
  val orderTransitiveOrder = law0(order, "transitive order")
  val orderOrderAndEqualConsistent = law0(order, "order and equal consistent")
  val orderConsistentScalaOrdering = law0(order, "consistent scala ordering")

  val enum = law("enum")
  val enumAll = all(enum)
  val enumSuccPred = law0(enum, "succ pred")
  val enumPredSucc = law0(enum, "pred succ")
  val enumMinMaxPred = law0(enum, "min max pred")
  val enumMinMaxSucc = law0(enum, "min max succ")
  val enumSuccN = law0(enum, "succ n")
  val enumPredN = law0(enum, "pred n")
  val enumSuccOrder = law0(enum, "succ order")
  val enumPredOrder = law0(enum, "pred order")


  val semigroup = law("semigroup")
  val semigroupAssociative = law0(semigroup, "associative")

  val monoid = law("monoid")
  val monoidAll = all(monoid)
  val monoidLeftIdentity = law0(monoid, "left identity")
  val monoidRightIdentity = law0(monoid, "right identity")


  val invariantFunctor = law("invariantFunctor")
  val invariantFunctorIdentity = law0(invariantFunctor, "identity")
  val invariantFunctorComposite = law0(invariantFunctor, "composite")

  val functor = law("functor")
  val functorAll = all(functor)
  val functorIdentity = law0(functor, "identity")
  val functorCompsite = law0(functor, "composite")

  val apply = law("apply")
  val applyAll = all(apply)
  val applyComposition = law0(apply, "composition")

  val applicative = law("applicative")
  val applicativeAll = all(applicative)
  val applicativeIdentity = law0(applicative, "identity")
  val applicativeHomomorphism = law0(applicative, "homomorphism")
  val applicativeInterchange = law0(applicative, "interchange")
  val applicativeMapConsistentWithAp = law0(applicative, "map consistent with ap")

  val applicativePlus = law("applicativePlus")

  val bind = law("bind")
  val bindAll = all(bind)
  val bindAssociativity = law0(bind, "associativity")
  val bindApConsistentWithBind = law0(bind, "consistent with bind")

  val monad = law("monad")
  val monadAll = all(monad)
  val monadRightIdentity = law0(monad, "right identity")
  val monadLeftIdentity = law0(monad, "left identity")

  val cobind = law("cobind")
  val cobindAll = all(cobind)
  val cobindAssociative = law0(cobind, "associative")

  val comonad = law("comonad")
  val comonadAll = all(comonad)
  val comonadLeftIdentity = law0(comonad, "left identity")
  val comonadRightIdentity = law0(comonad, "right identity")

  val plus = law("plus")
  val plusAll = all(plus)
  val plusAssociative = law0(plus, "associative")

  val plusEmpty = law("plusEmpty")
  val plusEmptyAll = all(plusEmpty)
  val plusEmptyLeftIdentity = law0(plusEmpty, "left identity")
  val plusEmptyRightIdentity = law0(plusEmpty, "right identity")

  val isEmpty = law("isEmpty")
  val isEmptyAll = all(isEmpty)
  val isEmptyEmptyIsEmpty = law0(isEmpty, "emptyIsEmpty")
  val isEmptyEmptyPlusIdentity = law0(isEmpty, "emptyPlusIdentity")

  val monadPlus = law("monadPlus")
  val monadPlusAll = all(monadPlus)
  val monadPlusEmptyMap = law0(monadPlus, "empty map")
  val monadPlusLeftZero = law0(monadPlus, "left zero")
  val monadPlusRightZero = law0(monadPlus, "right zero")

  val monadPlusStrong = law("monadPlusStrong")
  val monadPlusStrongAll = all(monadPlusStrong)

  val monadState = law("monadState")
  val monadStateAll = all(monadState)
  val monadStatePutPut = law0(monadState, "put put")
  val monadStatePutGet = law0(monadState, "put get")
  val monadStateGetPut = law0(monadState, "get put")
  val monadStateGetGet = law0(monadState, "get get")

  val align = law("align")
  val alignAll = all(align)
  val alignCollapse = law0(align, "collapse")

  val associative = law("associative")
  val associativeLeftRight = law0(associative, "left right")
  val associativeRightLeft = law0(associative, "right left")

  val contravariant = law("contravariant")
  val contravariantAll = all(contravariant)
  val contravariantIdentity = law0(contravariant, "identity")
  val contravariantComposite = law0(contravariant, "composite")

  val divide = law("divide")
  val divideAll = all(divide)
  val divideComposition = law0(divide, "composition")

  val divisible = law("divisible")
  val divisibleAll = all(divisible)
  val divisibleRightIdentity = law0(divisible, "right identity")
  val divisibleLeftIdentity = law0(divisible, "left identity")

  val foldable = law("foldable")
  val foldableLeftFMConsistent = law0(foldable, "left fold consistent foldMap")
  val foldableRightFMConsistent = law0(foldable, "right fold consistent foldMap")

  val foldable1 = law("foldable1")
  val foldable1All = all(foldable1)
  val foldable1LeftFM1Consistent = law0(foldable1, "left fold consistent foldMap1")
  val foldable1RightFM1Consistent = law0(foldable1, "right fold consistent foldMap1")

  val traverse = law("traverse")
  val traverseAll = all(traverse)
  val traverseIdentity = law0(traverse, "identity")
  val traversePurityMaybe = law0(traverse, "purity Maybe")
  val traversePurityIList = law0(traverse, "purity IList")
  val traverseSequentialFusion = law0(traverse, "sequential fusion")
  val traverseNaturality = law0(traverse, "naturality")
  val traverseParallelFusion = law0(traverse, "parallel fusion")

  val traverse1 = law("traverse1")
  val traverse1All = all(traverse1)
  val traverse1Identity = law0(traverse1, "identity")
  val traverse1SequentialFusion1 = law0(traverse1, "sequential fusion1")
  val traverse1Naturality1 = law0(traverse1, "naturality1")
  val traverse1ParallelFusion1 = law0(traverse1, "parallel fusion")

  val zip = law("zip")
  val zipPreservation = law0(zip, "preservation")
  val zipSymmetric = law0(zip, "symmetric")
  val zipApply = law0(zip, "zipApply")

  val monadError = law("monadError")
  val monadErrorAll = all(monadError)
  val monadErrorRaisedErrorsHandled = law0(monadError, "raised errors handled")
  val monadErrorErrorsRaised = law0(monadError, "errors raised")
  val monadErrorErrorsStopComputation = law0(monadError, "errors stop computation")

  val representable = law("representable")
  val representableRepUnrep = law0(representable, "rep unrep")
  val representableUnrepRep = law0(representable, "unrep rep")

  val compose = law("compose")
  val composeAll = all(compose)
  val composeAssociative = law0(compose, "associative")

  val category = law("category")
  val categoryAll = all(category)
  val categoryLeftIdentity = law0(category, "left identity")
  val categoryRightIdentity = law0(category, "right identity")

  val arrow = law("arrow")
  val arrowAll = all(arrow)
  val arrowIdentity = law0(arrow, "identity")
  val arrowComposition = law0(arrow, "composition")
  val arrowExtension = law0(arrow, "extension")
  val arrowFunctor = law0(arrow, "functor")
  val arrowExchange = law0(arrow, "exchange")
  val arrowUnit = law0(arrow, "unit")
  val arrowAssociation = law0(arrow, "association")


  val bifunctor = law("bifunctor")

  val bifoldable = law("bifoldable")
  val bifoldableAll = all(bifoldable)
  val bifoldableLeftFMConsistent = law0(bifoldable, "bifoldableLeftFMConsistent")
  val bifoldableRightFMConsistent = law0(bifoldable, "bifoldableRightFMConsistent")

  val bitraverse = law("bitraverse")
  val bitraverseAll = all(bitraverse)


  val profunctor = law("profunctor")


  val lens = law("lens")
  val lensIdentity = law0(lens, "identity")
  val lensRetention = law0(lens, "retentions")
  val lensDoubleSet = law0(lens, "double set")

  val iso = law("iso")
  val isoA2B = law0(iso, "a to b")
  val isoB2A = law0(iso, "b to a")

  val monadTrans = law("monadTrans")
  val monadTransLaw1Maybe = law0(monadTrans, "law1 Maybe")
  val monadTransLaw1IList = law0(monadTrans, "law1 IList")
  val monadTransLaw2Maybe = law0(monadTrans, "law2 Maybe")
  val monadTransLaw2IList = law0(monadTrans, "law2 IList")

  val comonadTrans = law("comonadTrans")
  val comonadTransLaw1Nel = law0(comonadTrans, "law1 NonEmptyList")
  val comonadTransLaw2Nel = law0(comonadTrans, "law2 NonEmptyList")

  val values: List[ScalazLaw] = set.toList

  implicit val scalazLawGen: Gen[ScalazLaw] = {
    val h :: t = values
    Gen.elements(h, t: _*)
  }

  implicit val scalazLawOrder: Order[ScalazLaw] = {
    import scalaz.std.anyVal._
    Order.orderBy(_.ord)
  }
}
