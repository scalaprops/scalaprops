package scalaprops

import scalaz._
import scalaz.std.AllInstances._
import FunctionEqual._

object CogenTest extends Scalaprops {
  private[this] def test[A: Gen: Cogen: Equal] = Property.forAll {
    Cogen[A].checkLaw
    true
  }

  private[this] def test1[F[_]](implicit
    G: Gen[F[Byte]],
    C: Cogen[F[Byte]],
    E: Equal[F[Byte]]
  ) = test[F[Byte]]

  private[this] def test2[F[_, _]](implicit
    G: Gen[F[Byte, Byte]],
    C: Cogen[F[Byte, Byte]],
    E: Equal[F[Byte, Byte]]
  ) = test[F[Byte, Byte]]

  val byte = test[Byte]
  val int = test[Byte]
  val list = test1[List]
  val ilist = test1[IList]
  val nel = test1[NonEmptyList]
  val tree = test1[Tree]
  val map = test2[Map]
  val tuple2 = test2[Tuple2]
  val maybeT = test[MaybeT[IList, Byte]]
  val imap = test2[IMap]

}
