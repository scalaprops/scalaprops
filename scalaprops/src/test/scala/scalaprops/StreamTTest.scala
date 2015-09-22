package scalaprops

import scalaz._
import scalaz.std.anyVal._
import scalaz.std.stream._
import scalaz.Isomorphism._

object StreamTTest extends Scalaprops {

  private[this] def iso[F[_]: Monad]: ({type l[a] = StreamT[F, a]})#l <~> ({type l[a] = F[Stream[a]]})#l =
    new IsoFunctorTemplate[({type l[a] = StreamT[F, a]})#l, ({type l[a] = F[Stream[a]]})#l] {
      override def to[A](fa: StreamT[F, A]) =
        fa.toStream

      override def from[A](ga: F[Stream[A]]) =
        StreamT.fromStream(ga)
    }

  val testId = {
    import scalaz.Id._
    type G[A] = Id[A]
    type F[A] = StreamT[G, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.iso.all(iso[G].unlift[Byte]),
      scalazlaws.equal.all[F[Int]]
    )
  }

  val testOneOrTwo = {
    type G[A] = OneAnd[Maybe, A]
    type F[A] = StreamT[G, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.iso.all(iso[G].unlift[Byte]),
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.maxSize(2))

  val testMaybe = {
    type G[A] = Maybe[A]
    type F[A] = StreamT[G, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.iso.all(iso[G].unlift[Byte]),
      scalazlaws.equal.all[F[Int]]
    )
  }

  val iList = {
    type G[A] = IList[A]
    type F[A] = StreamT[G, A]
    Properties.list(
      scalazlaws.monadPlus.all[F],
      scalazlaws.iso.all(iso[G].unlift[Byte]),
      scalazlaws.equal.all[F[Int]]
    )
  }.andThenParam(Param.maxSize(3))

  val monadTrans = scalazlaws.monadTrans.all[StreamT]

}
