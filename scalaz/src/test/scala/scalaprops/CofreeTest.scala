package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.stream._
import scalaz.std.vector._
import ScalapropsScalaz._
import CofreeTestInstances._

object CofreeTest extends Scalaprops {
  private[this] type CofreeZip[F[_], A] = Cofree[F, A] @@ Tags.Zip

  private def cofreeZipTest[F[_]: Applicative](
    implicit
    F1: Gen[CofreeZip[F, Int]],
    F2: Gen[CofreeZip[F, Int => Int]],
    F3: Equal[CofreeZip[F, Int]]
  ) = {
    type G[A] = CofreeZip[F, A]
    implicit val a: Applicative[({ type l[a] = Cofree[F, a] @@ Tags.Zip })#l] = Cofree.cofreeZipApplicative[F]
    scalazlaws.apply.all[G]
  }

  private[this] val strictTreeToCofreeIList: StrictTree ~> CofreeIList = {
    new (StrictTree ~> CofreeIList) {
      def apply[A](tree: StrictTree[A]) =
        Cofree(tree.rootLabel, IList.fromFoldable(tree.subForest).map(apply))
    }
  }

  val zipMaybe = {
    import CofreeGenImplicit._
    cofreeZipTest[Maybe]
  }

  val zipIList = {
    implicit def gen[A: Gen]: Gen[CofreeZip[IList, A]] =
      Tags.Zip.subst(Gen[StrictTree[A]].map(strictTreeToCofreeIList.apply))

    cofreeZipTest[IList]
  }

  val zipValidation = {
    import CofreeGenImplicit._
    type F[A] = ValidationNel[Byte, A]
    cofreeZipTest[F]
  }

  val zipDisjunction = {
    import CofreeGenImplicit._
    type F[A] = Byte \/ A
    cofreeZipTest[F]
  }

  val testMaybe = {
    implicit def genCofreeMaybe[A: Gen]: Gen[Cofree[Maybe, A]] =
      Gen[OneAnd[List, A]].map { list =>
        Cofree.unfold(list) {
          case OneAnd(a, h :: t) =>
            (a, Maybe.just(OneAnd(h, t)))
          case OneAnd(a, Nil) =>
            (a, Maybe.empty[OneAnd[List, A]])
        }
      }

    type F[A] = Cofree[Maybe, A]

    Properties.list(
      scalazlaws.monad.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Int]]
    )
  }

  private type CofreeStream[A] = Cofree[Stream, A]
  private type CofreeIList[A] = Cofree[IList, A]

  private[this] val treeToCofreeStream: Tree ~> CofreeStream =
    new (Tree ~> CofreeStream) { self =>
      def apply[A](tree: Tree[A]) =
        Cofree(tree.rootLabel, tree.subForest.map(self.apply))
    }

  val stream = {
    implicit def gen[A: Gen]: Gen[CofreeStream[A]] =
      Gen[Tree[A]].map(treeToCofreeStream(_))

    Properties.list(
      scalazlaws.monad.all[CofreeStream],
      scalazlaws.comonad.all[CofreeStream],
      scalazlaws.traverse1.all[CofreeStream],
      scalazlaws.equal.all[CofreeStream[Int]]
    )
  }

  val disjunction = {
    type E[A] = Byte \/ A
    type F[A] = Cofree[E, A]

    import CofreeGenImplicit._

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }

  val validation = {
    type E[A] = ValidationNel[Byte, A]
    type F[A] = Cofree[E, A]

    import CofreeGenImplicit._

    Properties.list(
      scalazlaws.bind.all[F],
      scalazlaws.comonad.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.equal.all[F[Byte]]
    )
  }
}
