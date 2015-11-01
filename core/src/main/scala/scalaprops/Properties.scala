package scalaprops

import scalaz._

final case class Properties[A] private (props: Tree[(A, Maybe[Check])]) {
  def id: A = props.rootLabel._1

  private[this] def map[B](f: (A, Maybe[Check]) => (B, Maybe[Check])): Properties[B] =
    Properties(props.map(f.tupled))

  def andThenParam(f: Endo[Param]): Properties[A] =
    mapCheck(_.map(p => p.copy(paramEndo = p.paramEndo andThen f)))

  def andThenParamPF(f: PartialFunction[A, Endo[Param]]): Properties[A] =
    Properties(props.map{
      case (i, Maybe.Just(m)) if f.isDefinedAt(i) =>
        i -> Maybe.just(Check(m.prop, m.paramEndo andThen f.apply(i)))
      case a => a
    })

  def composeParam(f: Endo[Param]): Properties[A] =
    mapCheck(_.map(p => p.copy(paramEndo = p.paramEndo compose f)))

  def mapId[B](f: A => B): Properties[B] =
    map((i, m) => (f(i), m))

  def mapRootId(f: A => A): Properties[A] =
    Properties(Tree.Node(f(props.rootLabel._1) -> props.rootLabel._2, props.subForest))

  def mapCheck(f: Maybe[Check] => Maybe[Check]): Properties[A] =
    map((i, m) => (i, f(m)))

  def ignore(reason: String): Properties[A] =
    mapCheck(_.map(_.ignore(reason)))

  def product[B](that: Properties[B]): Properties[Unit :-: A :-: B :-: Or.Empty] = {
    type T = Unit :-: A :-: B :-: Or.Empty
    Properties.noSort[T](
      Tree.Node(
        Or[T](()) -> Maybe.empty[Check],
        Stream(
          this.mapId(Or[T]._apply).props,
          that.mapId(Or[T]._apply).props
        )
      )
    )
  }
}

object Properties {
  def either[A: Order, B: Order](id: A, prop0: Properties[B], props: Properties[B] *): Properties[A :-: B :-: Or.Empty] = {
    type T = A :-: B :-: Or.Empty
    fromProps[T](
      Or[T](id),
      prop0.mapId(Or[T]._apply),
      props.map(_.mapId(Or[T]._apply)): _*
    )
  }

  def list[A: Order](prop0: Properties[A], props: Properties[A]*): Properties[Unit :-: A :-: Or.Empty] = {
    import scalaz.std.anyVal._
    either((), prop0, props: _*)
  }

  private[this] def ord1[A, B](implicit A: Order[A]): Order[(A, B)] =
    Order.orderBy(_._1)

  def single[A](id: A, c: Check): Properties[A] =
    Properties(Tree.Leaf(id -> Maybe.just(c)))

  def single[A](id: A, p: Property): Properties[A] =
    Properties(Tree.Leaf(id -> Maybe.just(p.toCheck)))

  private[this] def properties0[A: Order](id: A, nodes: Stream[Tree[(A, Maybe[Check])]]): Properties[A] =
    Properties(distinctTree(Tree.Node(
      id -> Maybe.empty[Check], nodes
    ))(ord1))

  private[scalaprops] def noSort[A](tree: Tree[(A, Maybe[Check])]): Properties[A] =
    Properties(tree)

  def properties[A: Order](id: A)(props: (A, Property) *): Properties[A] =
    properties0(
      id, props.map{case (n, p) => Tree.Leaf(n -> Maybe.just(p.toCheck))}(collection.breakOut)
    )

  def fromChecks[A: Order](id: A)(checks: (A, Check) *): Properties[A] =
    properties0(
      id, checks.map{case (n, p) => Tree.Leaf(n -> Maybe.just(p))}(collection.breakOut)
    )

  def fromProps[A: Order](id: A, prop0: Properties[A], props: Properties[A] *): Properties[A] =
    properties0(
      id, prop0.props #:: props.map(_.props).toStream
    )

  private[this] def distinctTree[A](tree: Tree[A])(implicit A: Order[A]): Tree[A] = {
    import std.stream._

    val x = Traverse[Tree].mapAccumL(tree, ISet.empty[A]) { (set, a) =>
      if (set.contains(a)) {
        (set, Maybe.empty[A])
      } else {
        (set.insert(a), Maybe.just(a))
      }
    }._2

    def loop(t: Tree[Maybe[A]]): Maybe[Tree[A]] =
      t.rootLabel.map{ root =>
        Tree.Node(root, MonadPlus[Stream].unite(t.subForest.map(loop)))
      }

    loop(x).toOption.get
  }
}
