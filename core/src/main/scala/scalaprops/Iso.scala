package scalaprops

import java.nio.ByteBuffer

import scalaz._
import scalaz.Isomorphism._

object Iso {

  private[this] val UnitR = \/-(())
  private[this] val UnitL = -\/(())

  val maybe: ({type l[a] = Unit \/ a})#l <~> Maybe =
    new IsoFunctorTemplate[({type l[a] = Unit \/ a})#l, Maybe] {
      override def from[A](fa: Maybe[A]) = fa.toRight(())

      override def to[A](ga: Unit \/ A) = ga match {
        case \/-(a) => Maybe.Just(a)
        case -\/(_) => Maybe.empty[A]
      }
    }

  val iList: IList <~> ({type l[a] = Unit \/ LazyTuple2[a, IList[a]]})#l =
    new IsoFunctorTemplate[IList, ({type l[a] = Unit \/ LazyTuple2[a, IList[a]]})#l] {
      def from[A](fa: Unit \/ LazyTuple2[A, IList[A]]) = fa match {
        case \/-(x) => x._1 :: x._2
        case -\/(_) => IList.empty[A]
      }

      def to[A](ga: IList[A]) = ga match {
        case ICons(h, t) => \/-(LazyTuple2(h, t))
        case INil() => UnitL
      }
    }

  val boolean: (Unit \/ Unit) <=> Boolean =
    new Iso[Function1, Unit \/ Unit, Boolean] {
      val from: Boolean => (Unit \/ Unit) = {
        case true => UnitR
        case false => UnitL
      }

      val to: (Unit \/ Unit => Boolean) = _.isRight
    }

  val bigInt: NonEmptyList[Byte] <=> BigInt =
    new Iso[Function1, NonEmptyList[Byte], BigInt] {
      override val from = { n: BigInt =>
        val array = n.toByteArray
        NonEmptyList.nel(array(0), IList.fromList(array.drop(1).toList))
      }
      override val to = { bytes: NonEmptyList[Byte] =>
        BigInt(bytes.list.toList.toArray)
      }
    }


  val long: BigInt <=> Long =
    new Iso[Function1, BigInt, Long] {
      override val from: Long => BigInt = BigInt(_)

      override val to: BigInt => Long = _.toLong
    }

  def tuple4[A1, A2, A3, A4]: LazyTuple4[A1, A2, A3, A4] <=> (A1, A2, A3, A4) =
    new IsoSet[LazyTuple4[A1, A2, A3, A4], (A1, A2, A3, A4)] {
      override val from = {
        x: (A1, A2, A3, A4) => LazyTuple4(x._1, x._2, x._3, x._4)
      }

      override val to = {
        x: LazyTuple4[A1, A2, A3, A4] => (x._1, x._2, x._3, x._4)
      }
    }

  type T4[A] = (A, A, A, A)

  val int: T4[Byte] <=> Int =
    new Iso[Function1, T4[Byte], Int] {
      override val to: T4[Byte] => Int = { n =>
        ByteBuffer.wrap(Array[Byte](n._1, n._2, n._3, n._4)).getInt
      }

      override val from: Int => T4[Byte] = { n =>
        val buf = ByteBuffer.allocate(4).putInt(n)
        (buf.get(0), buf.get(1), buf.get(2), buf.get(3))
      }
    }

  val nel: ({type l[a] = (a, List[a])})#l <~> NonEmptyList =
    new IsoFunctorTemplate[({type l[a] = (a, List[a])})#l, NonEmptyList] {
      def to[A](fa: (A, List[A])) =
        NonEmptyList.nel(fa._1, IList.fromList(fa._2))

      def from[A](ga: NonEmptyList[A]) =
        (ga.head, ga.tail.toList)
    }

  val either: \/ <~~> Either =
    new IsoBifunctorTemplate[\/, Either] {
      override def from[A, B](fa: Either[A, B]) =
        \/.fromEither(fa)

      override def to[A, B](ga: A \/ B) =
        ga.toEither
    }
}
