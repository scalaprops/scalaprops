package scalaprops

import Variant.variantInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

abstract class Cogen[A] { self =>
  def cogen[B](a: A, g: CogenState[B]): CogenState[B]

  final def contramap[B](f: B => A): Cogen[B] =
    new Cogen[B] {
      def cogen[C](a: B, g: CogenState[C]) =
        self.cogen(f(a), g)
    }
}

sealed abstract class CogenInstances0 extends CogenInstances {

  implicit def cogenList[A](implicit A: Cogen[A]): Cogen[List[A]] =
    new Cogen[List[A]] {
      def cogen[B](a: List[A], g: CogenState[B]) = a match {
        case h :: t =>
          variantInt(1, A.cogen(h, cogen(t, g)))
        case _ =>
          g
      }
    }

  implicit def cogenArray[A](implicit A: Cogen[A]): Cogen[Array[A]] =
    Cogen[List[A]].contramap(_.toList)
}

object Cogen extends CogenInstances0 {

  private[this] val byteArrayToIntList: Array[Byte] => List[Int] = { bytes =>
    val x =
      if (bytes.length % 4 == 0) {
        0
      } else {
        -1
      }
    var ints = List.empty[Int]
    val len = ints.length + x
    var i = 0
    while (i < len) {
      ints ::= ((bytes(i + 0) & 0xFF) << 24) |
        ((bytes(i + 1) & 0xFF) << 16) |
        ((bytes(i + 2) & 0xFF) << 8) |
        ((bytes(i + 3) & 0xFF) << 0)
      i += 1
    }
    if (x != 0) {
      (bytes.length % 4) match {
        case 1 =>
          ints ::= ((bytes(i + 0) & 0xFF) << 24)
        case 2 =>
          ints ::= ((bytes(i + 0) & 0xFF) << 24) |
            ((bytes(i + 1) & 0xFF) << 16)
        case 3 =>
          ints ::= ((bytes(i + 0) & 0xFF) << 24) |
            ((bytes(i + 1) & 0xFF) << 16) |
            ((bytes(i + 2) & 0xFF) << 8)
      }
    }
    ints
  }

  implicit def f1[A1, Z](implicit A1: Gen[A1], C: Cogen[Z]): Cogen[A1 => Z] =
    new Cogen[A1 => Z] {
      def cogen[X](f: A1 => Z, g: CogenState[X]) =
        CogenState(g.rand.next, A1.flatMap(x => C.cogen(f(x), g).gen))
    }

  def from[A1, Z](f: Z => Option[A1])(implicit A1: Cogen[A1]): Cogen[Z] =
    from1[A1, Z](f)(A1)

  def from1[A1, Z](f: Z => Option[A1])(implicit A1: Cogen[A1]): Cogen[Z] =
    A1.contramap(t => f(t).get)

  implicit val cogenBoolean: Cogen[Boolean] =
    new Cogen[Boolean] {
      def cogen[B](a: Boolean, g: CogenState[B]) =
        variantInt(if (a) 0 else 1, g)
    }

  implicit val cogenUnit: Cogen[Unit] =
    new Cogen[Unit] {
      def cogen[B](a: Unit, g: CogenState[B]) = g
    }

  implicit val cogenInt: Cogen[Int] =
    new Cogen[Int] {
      def cogen[B](a: Int, g: CogenState[B]) =
        variantInt(if (a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenByte: Cogen[Byte] =
    new Cogen[Byte] {
      def cogen[B](a: Byte, g: CogenState[B]) =
        variantInt(if (a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenShort: Cogen[Short] =
    new Cogen[Short] {
      def cogen[B](a: Short, g: CogenState[B]) =
        variantInt(if (a >= 0) 2 * a else -2 * a + 1, g)
    }

  implicit val cogenLong: Cogen[Long] =
    new Cogen[Long] {
      def cogen[B](a: Long, g: CogenState[B]) =
        Variant.variant(if (a >= 0L) 2L * a else -2L * a + 1L, g)
    }

  implicit val cogenChar: Cogen[Char] =
    new Cogen[Char] {
      def cogen[B](a: Char, g: CogenState[B]) =
        variantInt(a << 1, g)
    }

  implicit val cogenFloat: Cogen[Float] =
    Cogen[Long].contramap(java.lang.Float.floatToIntBits)

  implicit val cogenDouble: Cogen[Double] =
    Cogen[Long].contramap(java.lang.Double.doubleToLongBits)

  implicit val cogenByteArray: Cogen[Array[Byte]] =
    Cogen[List[Int]].contramap(byteArrayToIntList)

  implicit val cogenByteList: Cogen[List[Byte]] =
    Cogen[Array[Byte]].contramap(_.toArray)

  implicit val cogenJavaBoolean: Cogen[java.lang.Boolean] =
    Cogen[Boolean].contramap(_.booleanValue)

  implicit val cogenJavaInteger: Cogen[java.lang.Integer] =
    Cogen[Int].contramap(_.intValue)

  implicit val cogenJavaByte: Cogen[java.lang.Byte] =
    Cogen[Byte].contramap(_.byteValue)

  implicit val cogenJavaShort: Cogen[java.lang.Short] =
    Cogen[Short].contramap(_.shortValue)

  implicit val cogenJavaLong: Cogen[java.lang.Long] =
    Cogen[Long].contramap(_.longValue)

  implicit val cogenJavaCharacter: Cogen[java.lang.Character] =
    Cogen[Char].contramap(_.charValue)

  implicit val cogenJavaFloat: Cogen[java.lang.Float] =
    Cogen[Float].contramap(_.floatValue)

  implicit val cogenJavaDouble: Cogen[java.lang.Double] =
    Cogen[Double].contramap(_.doubleValue)

  import java.{math => jm}

  implicit val cogenBigInteger: Cogen[jm.BigInteger] =
    Cogen[Array[Byte]].contramap(_.toByteArray)

  implicit val cogenBigInt: Cogen[BigInt] =
    Cogen[Array[Byte]].contramap(_.toByteArray)

  implicit val cogenJavaBigDecimal: Cogen[jm.BigDecimal] =
    new Cogen[jm.BigDecimal] {
      def cogen[B](a: jm.BigDecimal, g: CogenState[B]) =
        Cogen[jm.BigInteger].cogen(a.unscaledValue, Cogen[Int].cogen(a.scale, g))
    }

  implicit val cogenBigDecimal: Cogen[BigDecimal] =
    Cogen[jm.BigDecimal].contramap(_.bigDecimal)

  implicit def cogenOption[A](implicit A: Cogen[A]): Cogen[Option[A]] =
    new Cogen[Option[A]] {
      def cogen[B](a: Option[A], g: CogenState[B]) = a match {
        case Some(o) =>
          variantInt(1, A.cogen(o, g))
        case None =>
          variantInt(g.rand.nextInt._2, g)
      }
    }

  implicit def cogenEither[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[Either[A, B]] =
    new Cogen[Either[A, B]] {
      def cogen[Z](a: Either[A, B], g: CogenState[Z]) = a match {
        case Right(x) =>
          variantInt(1, B.cogen(x, g))
        case Left(x) =>
          variantInt(0, A.cogen(x, g.copy(rand = g.rand.next)))
      }
    }

  implicit def cogenVector[A: Cogen]: Cogen[Vector[A]] = {
    Cogen[List[A]].contramap(_.toList)
  }

  implicit def cogenStream[A: Cogen]: Cogen[Stream[A]] = {
    Cogen[List[A]].contramap(_.toList)
  }

  implicit def cogenMap[A: Cogen, B: Cogen]: Cogen[Map[A, B]] =
    Cogen[List[(A, B)]].contramap(
      _.iterator.foldLeft(List.empty[(A, B)])(
        (list, keyValue) => keyValue :: list
      )
    )

  implicit def cogenSet[A: Cogen]: Cogen[Set[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit val cogenString: Cogen[String] =
    new Cogen[String] {
      def cogen[B](a: String, g: CogenState[B]) =
        cogenList(cogenChar).cogen(List(a.toCharArray: _*), g)
    }

  implicit val cogenSymbol: Cogen[Symbol] =
    Cogen[String].contramap(_.name)

  implicit def cogenFuture[A](implicit F: Cogen[A]): Cogen[Future[A]] = {
    import scala.concurrent.duration._
    val ec = scala.concurrent.ExecutionContext.global
    cogenEither(conquer[Throwable], F).contramap(
      f =>
        Await.result(
          f.map(Right(_))(ec).recover { case e => Left(e) }(ec),
          5.seconds
        )
    )
  }

  implicit def cogenTry[A](implicit F: Cogen[A]): Cogen[scala.util.Try[A]] =
    cogenEither(conquer[Throwable], F).contramap {
      case Success(a) => Right(a)
      case Failure(e) => Left(e)
    }

  implicit def cogenPartialFunction[A: Gen, B: Cogen]: Cogen[PartialFunction[A, B]] =
    Cogen[A => Option[B]].contramap(_.lift)

  implicit def cogenJavaEnum[A <: java.lang.Enum[A]]: Cogen[A] =
    Cogen[Int].contramap(_.ordinal)

  private[this] val empty = new Cogen[Any] {
    def cogen[X](a: Any, g: CogenState[X]) = g
  }
  def conquer[A] = empty.asInstanceOf[Cogen[A]]

  @deprecated(message = "Use divide instead", since = "0.6.3")
  def devide[A, B, C](fa: Cogen[A], fb: Cogen[B])(f: C => (A, B)): Cogen[C] =
    divide(fa, fb)(f)

  def divide[A, B, C](fa: Cogen[A], fb: Cogen[B])(f: C => (A, B)): Cogen[C] =
    new Cogen[C] {
      def cogen[X](c: C, g: CogenState[X]) = {
        val t = f(c)
        fa.cogen(t._1, fb.cogen(t._2, g))
      }
    }

  def apply[A](implicit A: Cogen[A]): Cogen[A] = A
}
