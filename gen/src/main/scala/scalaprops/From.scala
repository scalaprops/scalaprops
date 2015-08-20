package scalaprops

import scalaz._

abstract class From[F[_]]{
  def from[A](values: List[A]): Gen[F[A]]
}

object From {
  @inline def apply[F[_]](implicit F: From[F]): From[F] = F

  implicit val iList: From[IList] =
    new From[IList] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          Gen.ilist(Gen.elements(h, t : _*))
        case _ =>
          Gen.value(IList.empty)
      }
    }

  implicit val list: From[List] =
    new From[List] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          Gen.list(Gen.elements(h, t : _*))
        case _ =>
          Gen.value(Nil)
      }
    }

  implicit def eitherRight[B](implicit B: Gen[B]): From[({type l[a] = Either[B, a]})#l] =
    new From[({type l[a] = Either[B, a]})#l] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          val A = Gen.elements(h, t : _*)
          Gen.eitherGen(B, A)
        case _ =>
          B.map(Left(_))
      }
    }

  implicit def eitherLeft[B](implicit B: Gen[B]): From[({type l[a] = Either[a, B]})#l] =
    new From[({type l[a] = Either[a, B]})#l] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          val A = Gen.elements(h, t : _*)
          Gen.eitherGen(A, B)
        case _ =>
          B.map(Right(_))
      }
    }

  implicit def disjunctionRight[B](implicit B: Gen[B]): From[({type l[a] = B \/ a})#l] =
    new From[({type l[a] = B \/ a})#l] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          val A = Gen.elements(h, t : _*)
          Gen.disjunction(B, A)
        case _ =>
          B.map(\/.left)
      }
    }

  implicit def disjunctionLeft[B](implicit B: Gen[B]): From[({type l[a] = a \/ B})#l] =
    new From[({type l[a] = a \/ B})#l] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          val A = Gen.elements(h, t : _*)
          Gen.disjunction(A, B)
        case _ =>
          B.map(\/.right)
      }
    }

  implicit val option: From[Option] =
    new From[Option] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          Gen.option(Gen.elements(h, t : _*))
        case _ =>
          Gen.value(None)
      }
    }

  implicit def iMap[K: Order: Gen]: From[({type l[a] = K ==>> a})#l] =
    new From[({type l[a] = K ==>> a})#l] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          implicit val g = Gen.elements(h, t : _*)
          Gen[K ==>> A]
        case _ =>
          Gen.value(IMap.empty)
      }
    }

  implicit def map[K](implicit K: Gen[K]): From[({type l[a] = Map[K, a]})#l] =
    new From[({type l[a] = Map[K, a]})#l] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          val g = Gen.elements(h, t : _*)
          Gen.mapGen(K, g)
        case _ =>
          Gen.value(Map.empty)
      }
    }

  implicit val set: From[Set] =
    new From[Set] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          val g = Gen.elements(h, t : _*)
          Gen.setGen(g)
        case _ =>
          Gen.value(Set.empty)
      }
    }

  implicit def kleisli[F[_], A](implicit A: Cogen[A], F: Gen1[F], P: PlusEmpty[F]): From[({type l[a] = Kleisli[F, A, a]})#l] =
    new From[({type l[a] = Kleisli[F, A, a]})#l] {
      def from[C](values: List[C]) = values match {
        case h :: t =>
          implicit val g = F.gen1(Gen.elements(h, t : _*))
          Gen.kleisli[F, A, C]
        case _ =>
          Gen.value(Kleisli(_ => P.empty[C]))
      }
    }

  implicit val maybe: From[Maybe] =
    new From[Maybe] {
      def from[A](values: List[A]) = values match {
        case h :: t =>
          Gen.maybe(Gen.elements(h, t : _*))
        case _ =>
          Gen.value(Maybe.empty)
      }
    }
}
