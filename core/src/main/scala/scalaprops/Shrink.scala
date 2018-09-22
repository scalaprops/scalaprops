package scalaprops

import java.math.BigInteger
import scala.reflect.ClassTag

final class Shrink[A](val f: A => Stream[A]) {
  def apply(a: A): Stream[A] = f(a)

  def xmap[B](x: A => B, y: B => A): Shrink[B] =
    Shrink.shrink(y andThen f andThen(_.map(x)))
}

object Shrink {
  def apply[A](implicit A: Shrink[A]): Shrink[A] = A

  def shrink[A](f: A => Stream[A]): Shrink[A] =
    new Shrink(f)

  private[this] val Empty = shrink[Any](_ => Stream.Empty)

  def empty[A]: Shrink[A] = Empty.asInstanceOf[Shrink[A]]

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

  implicit val short: Shrink[Short] =
    long.xmap(_.toShort, x => x)

  implicit val byte: Shrink[Byte] =
    long.xmap(_.toByte, x => x)

  implicit def option[A](implicit A: Shrink[A]): Shrink[Option[A]] =
    shrink{
      case None => Stream.Empty
      case Some(a) => None #:: A(a).map(Option(_))
    }

  implicit def either[A, B](implicit A: Shrink[A], B: Shrink[B]): Shrink[A Either B] =
    shrink{
      case Left(a) =>
        A(a).map(Left(_))
      case Right(a) =>
        B(a).map(Right(_))
    }

  implicit def list[A](implicit A: Shrink[A]): Shrink[List[A]] = {

    def interleave[B](s1: Stream[B], s2: Stream[B]): Stream[B] =
      if (s1.isEmpty) s2
      else s1.head #:: interleave(s2, s1.tail)

    def removeChunks(n: Int, as: List[A]): Stream[List[A]] =
      as match {
        case Nil =>
          Stream.Empty
        case _ :: Nil =>
          Stream.cons(Nil, Stream.Empty)
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
                interleave(
                  removeChunks(n1, as1).withFilter(_.nonEmpty).map(_ ::: as2),
                  removeChunks(n2, as2).withFilter(_.nonEmpty).map(as1 ::: _)
                )
              )
            }
          )
      }

    def shrinkOne(as: List[A]): Stream[List[A]] = as match {
      case h :: t =>
        (A(h).map(_ :: t) ++ shrinkOne(t)).map(h :: _)
      case _ =>
        Stream.Empty
    }

    shrink(as => removeChunks(as.length, as) #::: shrinkOne(as))
  }

  implicit def stream[A: Shrink]: Shrink[Stream[A]] = {
    Shrink[List[A]].xmap(_.toStream, _.toList)
  }

  implicit def tuple2[A1, A2](implicit A1: Shrink[A1], A2: Shrink[A2]): Shrink[(A1, A2)] =
    shrink{ case (a1, a2) =>
      for {
        x1 <- A1(a1)
        x2 <- A2(a2)
      } yield (x1, x2)
    }

  implicit def tuple3[A1, A2, A3](implicit A1: Shrink[A1], A2: Shrink[A2], A3: Shrink[A3]): Shrink[(A1, A2, A3)] =
    shrink{ case (a1, a2, a3) =>
      for {
        x1 <- A1(a1)
        x2 <- A2(a2)
        x3 <- A3(a3)
      } yield (x1, x2, x3)
    }

  implicit def tuple4[A1, A2, A3, A4](implicit A1: Shrink[A1], A2: Shrink[A2], A3: Shrink[A3], A4: Shrink[A4]): Shrink[(A1, A2, A3, A4)] =
    shrink{ case (a1, a2, a3, a4) =>
      for {
        x1 <- A1(a1)
        x2 <- A2(a2)
        x3 <- A3(a3)
        x4 <- A4(a4)
      } yield (x1, x2, x3, x4)
    }

  implicit def map[A, B](implicit A: Shrink[A], B: Shrink[B]): Shrink[Map[A, B]] = {
    Shrink[List[(A, B)]].xmap(
      _.foldLeft(Map.newBuilder[A, B])(_ += _).result,
      _.foldRight(List.empty[(A, B)])(_ :: _)
    )
  }

  implicit def array[A: Shrink](implicit A: ClassTag[A]): Shrink[Array[A]] = {
    def ilist2array(list: List[A]) = {
      val array = new Array[A](list.length)
      @annotation.tailrec
      def loop(xs: List[A], i: Int): Unit = xs match {
        case h :: t =>
          array(i) = h
          loop(t, i + 1)
        case _ =>
      }
      loop(list, 0)
      array
    }

    Shrink[List[A]].xmap(ilist2array, _.toList)
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

  implicit def cogenShrink[A: Gen: Cogen]: Cogen[Shrink[A]] =
    Cogen[A => Stream[A]].contramap(_.f)

  implicit def shrinkGen[A: Cogen: Gen]: Gen[Shrink[A]] =
    Gen[A => Stream[A]].map(new Shrink(_))

}
