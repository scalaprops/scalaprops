package scalaprops

import java.math.BigInteger

import scala.reflect.ClassTag
import scalaz._
import scalaz.std.stream._
import scalaz.Isomorphism.<=>

final class Shrink[A](val f: A => Stream[A]) {
  def apply(a: A): Stream[A] = f(a)

  def xmap[B](x: A => B, y: B => A): Shrink[B] =
    Shrink.shrink(y andThen f andThen(_.map(x)))

  def xmapi[B](f: A <=> B): Shrink[B] =
    xmap(f.to, f.from)
}

object Shrink {
  def apply[A](implicit A: Shrink[A]): Shrink[A] = A

  def shrink[A](f: A => Stream[A]): Shrink[A] =
    new Shrink(f)

  private[this] val Empty = shrink[Any](_ => Stream.Empty)

  def empty[A]: Shrink[A] = Empty.asInstanceOf[Shrink[A]]

  implicit val shrinkInstance: InvariantFunctor[Shrink] =
    new InvariantFunctor[Shrink] {
      def xmap[A, B](ma: Shrink[A], f: A => B, g: B => A) =
        ma.xmap(f, g)
    }

  implicit val long: Shrink[Long] =
    shrink{
      case 0L => Stream.Empty
      case i =>
        val is = 0L #:: Stream.iterate(i)(_ / 2L).takeWhile(_ != 0L).map(i - _)
        if(i < 0L){
          -i #:: is
        } else {
          is
        }
    }

  implicit val boolean: Shrink[Boolean] =
    shrink(_ => Stream.cons(false, Stream.Empty))

  implicit val int: Shrink[Int] =
    long.xmap(_.toInt, x => x)

  implicit val byte: Shrink[Byte] =
    long.xmap(_.toByte, x => x)

  implicit def option[A](implicit A: Shrink[A]): Shrink[Option[A]] =
    shrink{
      case None => Stream.Empty
      case Some(a) => None #:: A(a).map(Option(_))
    }

  implicit def disjunction[A, B](implicit A: Shrink[A], B: Shrink[B]): Shrink[A \/ B] =
    shrink{
      case -\/(a) => A(a).map(\/.left)
      case \/-(a) => B(a).map(\/.right)
    }

  implicit def either[A: Shrink, B: Shrink]: Shrink[A Either B] =
    Shrink[A \/ B].xmap(_.toEither, \/.fromEither)

  implicit def ilist[A](implicit A: Shrink[A]): Shrink[IList[A]] = {
    def removeChunks(n: Int, as: IList[A]): Stream[IList[A]] =
      as match {
        case INil() =>
          Stream.Empty
        case ICons(a, INil()) =>
          Stream.cons(INil(), Stream.Empty)
        case _ =>
          val n1 = n / 2
          val n2 = n - n1
          val as1 = as.take(n1)
          Stream.cons(
            as1,
            {
              val as2 = as.drop(n1)
              Stream.cons(
                as2,
                std.stream.interleave(
                  removeChunks(n1, as1).withFilter(_.nonEmpty).map(_ ::: as2),
                  removeChunks(n2, as2).withFilter(_.nonEmpty).map(as1 ::: _)
                )
              )
            }
          )
      }

    def shrinkOne(as: IList[A]): Stream[IList[A]] = as match {
      case INil() => Stream.Empty
      case ICons(h, t) =>
        (A(h).map(_ :: t) ++ shrinkOne(t)).map(h :: _)
    }

    shrink(as => removeChunks(as.length, as) #::: shrinkOne(as))
  }

  implicit def list[A: Shrink]: Shrink[List[A]] =
    Shrink[IList[A]].xmap(_.toList, Gen.IListFromList)

  implicit def stream[A: Shrink]: Shrink[Stream[A]] = {
    Shrink[IList[A]].xmap(_.toStream, IList.fromFoldable(_))
  }

  implicit def tuple2[A1, A2](implicit A1: Shrink[A1], A2: Shrink[A2]): Shrink[(A1, A2)] =
    shrink{ case (a1, a2) =>
      Apply[Stream].tuple2(A1(a1), A2(a2))
    }

  implicit def tuple3[A1, A2, A3](implicit A1: Shrink[A1], A2: Shrink[A2], A3: Shrink[A3]): Shrink[(A1, A2, A3)] =
    shrink{ case (a1, a2, a3) =>
      Apply[Stream].tuple3(A1(a1), A2(a2), A3(a3))
    }

  implicit def tuple4[A1, A2, A3, A4](implicit A1: Shrink[A1], A2: Shrink[A2], A3: Shrink[A3], A4: Shrink[A4]): Shrink[(A1, A2, A3, A4)] =
    shrink{ case (a1, a2, a3, a4) =>
      Apply[Stream].tuple4(A1(a1), A2(a2), A3(a3), A4(a4))
    }

  implicit def map[A, B](implicit A: Shrink[A], B: Shrink[B]): Shrink[Map[A, B]] = {
    import syntax.foldable._
    Shrink[IList[(A, B)]].xmap(
      _.to[({type l[_] = Map[A, B]})#l],
      _.foldRight(IList.empty[(A, B)])(_ :: _)
    )
  }

  implicit def array[A: Shrink](implicit A: ClassTag[A]): Shrink[Array[A]] = {
    def ilist2array(list: IList[A]) = {
      val array = new Array[A](list.length)
      @annotation.tailrec
      def loop(xs: IList[A], i: Int): Unit = xs match {
        case ICons(h, t) =>
          array(i) = h
          loop(t, i + 1)
        case INil() =>
      }
      loop(list, 0)
      array
    }

    Shrink[IList[A]].xmap(ilist2array, array => IList(array: _*))
  }

  implicit val bigInteger: Shrink[BigInteger] =
    Shrink[(Byte, Array[Byte])].xmap(
      bs => {
        val x = new Array[Byte](bs._2.length - 1)
        var i = 0
        while(i < bs._2.length){
          x(i) = bs._2.apply(i)
          i += 1
        }
        x(bs._2.length) = bs._1
        new BigInteger(x)
      },
      i => {
        val b = i.toByteArray
        val x = new Array[Byte](b.length - 1)
        System.arraycopy(b, 0, x, 0, b.length - 1)
        (b(0), x)
      }
    )

  implicit val bigInt: Shrink[BigInt] =
    Shrink[BigInteger].xmap(BigInt(_), _.bigInteger)


}
