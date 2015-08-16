package scalaprops

import scalaz._
import scalaz.Isomorphism._

abstract class ToFunc[A] { self =>
  def toFunc[B](f: A => B): Func[A, B]

  final def xmap[B](f: A => B, g: B => A): ToFunc[B] =
    new ToFunc[B] {
      def toFunc[C](h: B => C) =
        ToFunc.functionMap[B, A, C](g, f, h)(self)
    }

  final def xmapi[B](iso: A <=> B): ToFunc[B] =
    xmap(iso.to, iso.from)
}

object ToFunc {
  @inline def apply[A](implicit A: ToFunc[A]): ToFunc[A] = A

  implicit val toFuncInstance: InvariantFunctor[ToFunc] =
    new InvariantFunctor[ToFunc] {
      def xmap[A, B](ma: ToFunc[A], f: A => B, g: B => A) =
        ma.xmap(f, g)
    }

  def functionMap[A, B, C](g: A => B, h: B => A, f: A => C)(implicit B: ToFunc[B]): Func[A, C] =
    Func.Map(g, h, B.toFunc(f compose h))

  def functionMapIso[A, B, C](iso: A <=> B, f: A => C)(implicit B: ToFunc[B]): Func[A, C] =
    functionMap(iso.to, iso.from, f)

  implicit val unit: ToFunc[Unit] =
    new ToFunc[Unit] {
      def toFunc[B](f: Unit => B) =
        Func.Single(f(()))
    }

  implicit val boolean: ToFunc[Boolean] =
    ToFunc[Unit \/ Unit].xmapi(Iso.boolean)

  implicit val byte: ToFunc[Byte] =
    new ToFunc[Byte] {
      import scalaz.std.anyVal._
      def toFunc[B](f: Byte => B) =
        Func.Table(
          (Byte.MinValue to Byte.MaxValue).map{ x =>
            LazyTuple2(x.asInstanceOf[Byte], f(x.asInstanceOf[Byte]))
          }.toStream
        )
    }

  implicit val short: ToFunc[Short] =
    new ToFunc[Short] {
      import scalaz.std.anyVal._
      def toFunc[B](f: Short => B) =
        Func.Table(
          (Short.MinValue to Short.MaxValue).map{ x =>
            LazyTuple2(x.asInstanceOf[Short], f(x.asInstanceOf[Short]))
          }.toStream
        )
    }

  implicit val int: ToFunc[Int] =
    ToFunc[Iso.T4[Byte]].xmapi(Iso.int)

  implicit def lazyTuple2[A1, A2](implicit
    A1: ToFunc[A1],
    A2: ToFunc[A2]
  ): ToFunc[LazyTuple2[A1, A2]] = new ToFunc[LazyTuple2[A1, A2]] {
    def toFunc[B](f: LazyTuple2[A1, A2] => B) =
      Func.Pair {
        A1.toFunc { a1 =>
          A2.toFunc { a2 =>
            f(LazyTuple2(a1, a2))
          }
        }
      }
  }

  implicit def lazyTuple3[A1: ToFunc, A2: ToFunc, A3: ToFunc]: ToFunc[LazyTuple3[A1, A2, A3]] =
    ToFunc[LazyTuple2[A1, LazyTuple2[A2, A3]]].xmap(
      x => LazyTuple3(x._1, x._2._1, x._2._2),
      x => LazyTuple2(x._1, LazyTuple2(x._2, x._3))
    )

  implicit def lazyTuple4[A1: ToFunc, A2: ToFunc, A3: ToFunc, A4: ToFunc]: ToFunc[LazyTuple4[A1, A2, A3, A4]] =
    ToFunc[LazyTuple2[LazyTuple2[A1, A2], LazyTuple2[A3, A4]]].xmap(
      x => LazyTuple4(x._1._1, x._1._2, x._2._1, x._2._2),
      x => LazyTuple2(LazyTuple2(x._1, x._2), LazyTuple2(x._3, x._4))
    )

  implicit def tuple4[A1: ToFunc, A2: ToFunc, A3: ToFunc, A4: ToFunc]: ToFunc[(A1, A2, A3, A4)] =
    ToFunc[LazyTuple4[A1, A2, A3, A4]].xmapi(Iso.tuple4[A1, A2, A3, A4])

  implicit def either[A1: ToFunc, A2: ToFunc]: ToFunc[Either[A1, A2]] =
    ToFunc[A1 \/ A2].xmapi(scalaprops.Iso.either.unlift)

  implicit def disjunction[A1, A2](implicit
    A1: ToFunc[A1],
    A2: ToFunc[A2]
  ): ToFunc[A1 \/ A2] = new ToFunc[A1 \/ A2] {
    def toFunc[B](f: (A1 \/ A2) => B) =
      Func.Sum(
        A1.toFunc(f.compose(\/.left)),
        A2.toFunc(f.compose(\/.right))
      )
  }

  implicit def maybe[A](implicit A: ToFunc[A]): ToFunc[Maybe[A]] =
    ToFunc[Unit \/ A].xmapi(Iso.maybe.unlift)

  implicit def iList[A](implicit A: ToFunc[A]): ToFunc[IList[A]] =
    new ToFunc[IList[A]] {
      def toFunc[B](f: IList[A] => B) =
        functionMapIso(Iso.iList.unlift, f)
    }

  implicit def list[A](implicit A: ToFunc[A]): ToFunc[List[A]] =
    ToFunc[IList[A]].xmapi(IList.listIListIso.flip.unlift[A])

}
