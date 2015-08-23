package scalaprops

import scalaz._
import scalaz.std.tuple.tuple2Equal
import scalaz.std.anyVal._

object ProductTest extends Scalaprops {

  private[this] type DByte[a] = Byte \/ a

  val maybeMaybe = {
    type F[A] = (Maybe[A], Maybe[A])

    implicit val instance1: ApplicativePlus[F] =
      ApplicativePlus[Maybe].product[Maybe]

    implicit val instance2: Traverse[F] =
      Traverse[Maybe].product[Maybe]

    implicit val instance3: Align[F] =
      Align[Maybe].product[Maybe]

    Properties.list(
      scalazlaws.applicativePlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.align.all[F]
    )
  }

  val maybeIList = {
    type F[A] = (Maybe[A], IList[A])

    implicit val instance1: ApplicativePlus[F] =
      ApplicativePlus[Maybe].product[IList]

    implicit val instance2: Traverse[F] =
      Traverse[Maybe].product[IList]

    implicit val instance3: Align[F] =
      Align[Maybe].product[IList]

    Properties.list(
      scalazlaws.applicativePlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.align.all[F]
    )
  }

  val iListIList = {
    type F[A] = (IList[A], IList[A])

    implicit val instance1: ApplicativePlus[F] =
      ApplicativePlus[IList].product[IList]

    implicit val instance2: Traverse[F] =
      Traverse[IList].product[IList]

    implicit val instance3: Align[F] =
      Align[IList].product[IList]

    Properties.list(
      scalazlaws.applicativePlus.all[F],
      scalazlaws.traverse.all[F],
      scalazlaws.align.all[F]
    )
  }

  val disjunction2 = {
    type F[A] = (DByte[A], DByte[A])

    implicit val instance1: Applicative[F] =
      Applicative[DByte].product[DByte]

    implicit val instance2: Traverse[F] =
      Traverse[DByte].product[DByte]

    implicit val instance3: Plus[F] =
      Plus[DByte].product[DByte]

    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.plus.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val maybeDisjuction = {
    type F[A] = (Maybe[A], DByte[A])

    implicit val instance1: Applicative[F] =
      Applicative[Maybe].product[DByte]

    implicit val instance2: Traverse[F] =
      Traverse[Maybe].product[DByte]

    implicit val instance3: Plus[F] =
      Plus[Maybe].product[DByte]

    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.plus.all[F],
      scalazlaws.traverse.all[F]
    )
  }

  val nelNel = {
    type F[A] = (NonEmptyList[A], NonEmptyList[A])

    implicit val instance1: Applicative[F] =
      Applicative[NonEmptyList].product[NonEmptyList]

    implicit val instance2: Traverse1[F] =
      Traverse1[NonEmptyList].product[NonEmptyList]

    implicit val instance3: Plus[F] =
      Plus[NonEmptyList].product[NonEmptyList]

    implicit val instance4: Align[F] =
      Align[NonEmptyList].product[NonEmptyList]

    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.plus.all[F],
      scalazlaws.align.all[F]
    )
  }

  val treeNel = {
    type F[A] = (Tree[A], NonEmptyList[A])

    implicit val instance1: Applicative[F] =
      Applicative[Tree].product[NonEmptyList]

    implicit val instance2: Traverse1[F] =
      Traverse1[Tree].product[NonEmptyList]

    implicit val instance3: Align[F] =
      Align[Tree].product[NonEmptyList]

    Properties.list(
      scalazlaws.applicative.all[F],
      scalazlaws.traverse1.all[F],
      scalazlaws.align.all[F]
    )
  }

}
