package scalaprops

import scalaz._

final case class Fun[A, B](f: Func[A, B], b: B, fun: A => B) {
  override def toString = "Fun(" + Fun.show(Show.showA[A], Show.showA[B]).shows(this) + ")"
}

object Fun {

  implicit def show[A, B](implicit A: Show[A], B: Show[B]): Show[Fun[A, B]] =
    Show.shows{ f =>
      f.f.string(Maybe.Just(f.b))
    }

  implicit def gen[A, B](implicit F: ToFunc[A], A: Cogen[A], B: Gen[B]): Gen[Fun[A, B]] =
    Apply[Gen].apply2(
      Gen[Func[A, B]], B
    )(_.mkFun(_))


  implicit def shrink[A, B](implicit B: Shrink[B]): Shrink[Fun[A, B]] =
    Shrink.shrink[Fun[A, B]]{ case Fun(p1, d1, _) =>
      Shrink[((Func[A, B]), B)].f((p1, d1)).map{
        case (p2, d2) => p2.mkFun(d2)
      }
    }
}
