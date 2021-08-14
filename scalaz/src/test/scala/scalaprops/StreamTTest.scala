package scalaprops

import scalaz._
import scalaz.std.AllInstances._
import scalaz.Isomorphism._
import scalaz.syntax.equal._
import scalaz.Id.Id
import Property.forAll
import scala.util.Try
import ScalapropsScalaz._

object StreamTTest extends Scalaprops {
  private[this] def iso[F[_]: Monad]: ({ type l[a] = StreamT[F, a] })#l <~> ({ type l[a] = F[LazyList[a]] })#l =
    new IsoFunctorTemplate[({ type l[a] = StreamT[F, a] })#l, ({ type l[a] = F[LazyList[a]] })#l] {
      override def to_[A](fa: StreamT[F, A]) =
        fa.toLazyList

      override def from_[A](ga: F[LazyList[A]]) =
        StreamT.fromLazyList(ga)
    }

  private[this] type EitherByte[A] = Byte \/ A

  val unconsRec = {
    type A = Byte

    def test[F[_]: Monad: BindRec](implicit
      E: Equal[F[Option[(A, StreamT[F, A])]]],
      G: Gen[StreamT[F, A]]
    ) = forAll { (s: StreamT[F, A]) => s.uncons === s.unconsRec }

    Properties.properties("unconsRec")(
      "Id" -> test[Id],
      "Maybe" -> test[Maybe],
      "IList" -> test[IList],
      "Disjunction" -> test[EitherByte]
    )
  }

  val isEmptyRec = {
    type A = Byte

    def test[F[_]: Monad: BindRec](implicit
      E: Equal[F[Boolean]],
      G: Gen[StreamT[F, A]]
    ) = forAll { (s: StreamT[F, A]) => s.isEmpty === s.isEmptyRec }

    Properties.properties("isEmptyRec")(
      "Id" -> test[Id],
      "Maybe" -> test[Maybe],
      "IList" -> test[IList],
      "Disjunction" -> test[EitherByte]
    )
  }

  val headOptionRec = {
    type A = Byte

    def test[F[_]: Monad: BindRec](implicit
      E: Equal[F[Option[A]]],
      G: Gen[StreamT[F, A]]
    ) = forAll { (s: StreamT[F, A]) => s.headOption === s.headOptionRec }

    Properties.properties("headOptionRec")(
      "Id" -> test[Id],
      "Maybe" -> test[Maybe],
      "IList" -> test[IList],
      "Disjunction" -> test[EitherByte]
    )
  }

  val tailMRec = {
    type A = Byte

    def test[F[_]: Monad: BindRec](implicit
      E: Equal[F[StreamT[F, A]]],
      G: Gen[StreamT[F, A]]
    ) = forAll { (s: StreamT[F, A]) => Try(s.tailM).toOption === Try(s.tailMRec).toOption }

    Properties.properties("tailMRec")(
      "Id" -> test[Id],
      "Maybe" -> test[Maybe],
      "IList" -> test[IList],
      "Disjunction" -> test[EitherByte]
    )
  }

  val foldLeftRec = {
    type A = Byte

    def test[F[_]: Monad: BindRec](implicit
      G: Gen[StreamT[F, A]],
      E: Equal[F[A]]
    ) = forAll { (s: StreamT[F, A], z: A, f: (A, A) => A) => s.foldLeft(z)(f) === s.foldLeftRec(z)(f) }

    Properties.properties("foldLeftRec")(
      "Id" -> test[Id],
      "Maybe" -> test[Maybe],
      "IList" -> test[IList],
      "Disjunction" -> test[EitherByte]
    )
  }

  val foldRightRec = {
    type A = Byte

    def test[F[_]: Monad: BindRec](implicit
      G: Gen[StreamT[F, A]],
      E: Equal[F[A]]
    ) =
      forAll { (s: StreamT[F, A], z: A, f: (A, A) => A) =>
        s.foldRight(z)((x, y) => f(x, y)) === s.foldRightRec(z)((x, y) => f(x, y))
      }

    Properties.properties("foldRightRec")(
      "Id" -> test[Id],
      "Maybe" -> test[Maybe],
      "IList" -> test[IList],
      "Disjunction" -> test[EitherByte]
    )
  }

  val toStreamRec = {
    type A = Byte

    def test[F[_]: Monad: BindRec](implicit
      G: Gen[StreamT[F, A]],
      E: Equal[F[LazyList[A]]]
    ) = forAll { (s: StreamT[F, A]) => s.toLazyList === s.toLazyListRec }

    Properties.properties("toStreamRec")(
      "Id" -> test[Id],
      "Maybe" -> test[Maybe],
      "IList" -> test[IList],
      "Disjunction" -> test[EitherByte]
    )
  }

  val lengthRec = {
    type A = Byte

    def test[F[_]: Monad: BindRec](implicit
      G: Gen[StreamT[F, A]],
      E: Equal[F[Int]]
    ) = forAll { (s: StreamT[F, A]) => s.length === s.lengthRec }

    Properties.properties("lengthRec")(
      "Id" -> test[Id],
      "Maybe" -> test[Maybe],
      "IList" -> test[IList],
      "Disjunction" -> test[EitherByte]
    )
  }

  val testId = {
    import scalaz.Id._
    type G[A] = Id[A]
    type F[A] = StreamT[G, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.foldable.all[F],
      scalazlaws.iso.all(iso[G].unlift[Byte]),
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testOneOrTwo = {
    type G[A] = OneAnd[Maybe, A]
    type F[A] = StreamT[G, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.foldable.all[F],
      scalazlaws.iso.all(iso[G].unlift[Byte]),
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(
    Param.maxSize(2)
  ).andThenParamPF { case Or.R(Or.L(ScalazLaw.applyComposition | ScalazLaw.bindAssociativity)) =>
    Param.minSuccessful(1)
  }

  val testMaybe = {
    type G[A] = Maybe[A]
    type F[A] = StreamT[G, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.foldable.all[F],
      scalazlaws.iso.all(iso[G].unlift[Byte]),
      scalazlaws.equal.all[F[Int]]
    )
  }

  val iList = {
    type G[A] = IList[A]
    type F[A] = StreamT[G, A]
    Properties.list(
      scalazlaws.monadPlus.all[F].andThenParam(Param.minSuccessful(10)),
      scalazlaws.foldable.all[F],
      scalazlaws.iso.all(iso[G].unlift[Byte]),
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.maxSize(3)).andThenParamPF {
    case Or.R(Or.L(ScalazLaw.applyComposition | ScalazLaw.bindAssociativity)) =>
      Param.minSuccessful(1)
  }

  val monadTrans = scalazlaws.monadTrans.all[StreamT]
}
