package scalaprops

import scalaz._

/**
 * @see [[https://github.com/nick8325/quickcheck/blob/2.8.1/Test/QuickCheck/Function.hs]]
 */
sealed abstract class Func[A, B] extends Product with Serializable {
  def map[C](f: B => C): Func[A, C]
  def toAbstract(d: B): A => B

  final protected def notNil: Boolean = this.isInstanceOf[Func.Nil[_, _]] == false

  final def mkFun(b: B): Fun[A, B] =
    Fun(this, b, toAbstract(b))

  final def string(b: Maybe[B])(implicit A: Show[A], B: Show[B]): String = {
    val default = b.map(b => "  case _ => " + B.shows(b)).getOrElse("")

    table.toList.groupBy(_._2).toList.sortBy(_._2.size).foldLeft(
      new java.lang.StringBuilder("\n{")
    ){
      case (builder, (k, v)) =>
        val str = v.map(_._1).mkString("\n  case "," | "," => ") + k + ""
        builder.append(str).append("")
    }.append("\n" + default + "\n}\n").toString
  }

  def table: Stream[LazyTuple2[A, B]]

  def shrink(shr: B => Stream[B]): Stream[Func[A, B]]

  override final def toString = string(Maybe.empty[B])(Show.showA, Show.showA)
}

object Func {

  private def single[A]: A => Func[Unit, A] = Single(_)

  private[this] def mapR[A, B, C](f: A => B): LazyTuple2[C, A] => LazyTuple2[C, B] =
    t => LazyTuple2(t._1, f(t._2))

  private[this] def mapL[A, B, C](f: A => B): LazyTuple2[A, C] => LazyTuple2[B, C] =
    t => LazyTuple2(f(t._1), t._2)

  private[scalaprops] final case class Pair[A, B, C](a: Func[A, Func[B, C]]) extends Func[LazyTuple2[A, B], C] {
    override def map[D](f: C => D): Func[LazyTuple2[A, B], D] =
      Pair(a.map(_.map(f)))

    override def toAbstract(d: C): LazyTuple2[A, B] => C = {
      t => a.map{
        _.toAbstract(d)(t._2)
      }.toAbstract(d)(t._1)
    }

    override def table =
      a.table.flatMap{ x =>
        x._2.table.map{ y =>
          LazyTuple2(LazyTuple2(x._1, y._1), y._2)
        }
      }

    override def shrink(shr: C => Stream[C]) =
      a.shrink(_.shrink(shr)).map{
        case Func.Nil() => Func.Nil[LazyTuple2[A, B], C]()
        case p => Pair(p)
      }

  }

  private[scalaprops] final case class Sum[A, B, C](x: Func[A, C], y: Func[B, C]) extends Func[A \/ B, C] {
    override def map[D](f: C => D): Func[A \/ B, D] =
      Sum(x.map(f), y.map(f))

    override def toAbstract(d: C): (A \/ B) => C = {
      case -\/(a) => x.toAbstract(d)(a)
      case \/-(b) => y.toAbstract(d)(b)
    }

    import scalaz.syntax.either._

    override def table =
      x.table.map{
        mapL(_.left[B])
      } #::: y.table.map{
        mapL(_.right[A])
      }

    override def shrink(shr: C => Stream[C]): Stream[Func[A \/ B, C]] = {
      type T = Func[A \/ B, C]

      def +++(aa: Func[A, C], bb: Func[B, C]): T = (aa, bb) match {
        case (Func.Nil(), Func.Nil()) => Func.Nil[A \/ B, C]()
        case _ => Sum(aa, bb)
      }

      (if(y.notNil) Stream[T](+++(x, Func.Nil())) else Stream.empty[T]) #:::
      (if(x.notNil) Stream[T](+++(Func.Nil(), y)) else Stream.empty[T]) #:::
      y.shrink(shr).map(q => +++(x, q)) #:::
      x.shrink(shr).map(p => +++(p, y))
    }

  }

  private[scalaprops] final case class Single[A](a: A) extends Func[Unit, A] {
    override def map[C](f: A => C) =
      Single(f(a))

    override def toAbstract(d: A) =
      _ => a

    override def table =
      Stream(LazyTuple2((), a))

    override def shrink(shr: A => Stream[A]) =
      Func.Nil[Unit, A]() #:: shr(a).map(Func.single)
  }

  private[scalaprops] final case class Nil[A, B]() extends Func[A, B] {
    override def map[C](f: B => C): Func[A, C] =
      Nil()

    override def toAbstract(d: B) =
      _ => d

    override def table = Stream.empty

    override def shrink(shr: B => Stream[B]) =
      Stream.Empty
  }

  private[scalaprops] final case class Table[A, B](a: Stream[LazyTuple2[A, B]])(implicit val A: Equal[A]) extends Func[A, B] {
    override def map[C](f: B => C): Func[A, C] =
      Table(a.map(mapR(f)))

    override def toAbstract(d: B): A => B =
      aa => a.collectFirst{
        case x if A.equal(aa, x._1) => x._2
      }.getOrElse(d)

    override def table = a

    override def shrink(shr: B => Stream[B]): Stream[Func[A, B]] =
      Shrink.stream(
        new Shrink[LazyTuple2[A, B]](
          t => shr(t._2).map(a => LazyTuple2(t._1, a))
        )
      ).apply(a).map{
        case Stream.Empty => Func.Nil[A, B]()
        case s => Table(s)
      }
  }

  private[scalaprops] final case class Map[A, B, C](
    x: A => B, y: B => A, z: Func[B, C]
  ) extends Func[A, C] {

    override def map[D](f: C => D): Func[A, D] =
      Func.Map(x, y, z.map(f))

    override def toAbstract(d: C): A => C =
      x andThen z.toAbstract(d)

    override def table =
      z.table.map(mapL(y))

    override def shrink(shr: C => Stream[C]): Stream[Func[A, C]] =
      z.shrink(shr).map{
        case Func.Nil() => Func.Nil[A, C]()
        case p => Map(x, y, p)
      }
  }

  implicit def funcFunctor[C]: Functor[({type l[a] = Func[C, a]})#l] =
    new Functor[({type l[a] = Func[C, a]})#l] {
      def map[A, B](fa: Func[C, A])(f: A => B) =
        fa map f
    }

  implicit def show[A, B](implicit A: Show[A], B: Show[B]): Show[Func[A, B]] =
    Show.shows(_.string(Maybe.empty[B]))

  implicit def gen[A, B](implicit F: ToFunc[A], A: Cogen[A], B: Gen[B]): Gen[Func[A, B]] =
    Gen[A => B].map(F.toFunc)

  implicit def equal[A: Equal, B: Equal]: Equal[Func[A, B]] = {
    import scalaz.std.tuple._
    import scalaz.std.stream._
    Equal.equalBy(_.table.map(x => (x._1, x._2)))
  }

  implicit def shrink[A, B](implicit B: Shrink[B]): Shrink[Func[A, B]] =
    new Shrink[Func[A, B]](_.shrink(B.f))
}
