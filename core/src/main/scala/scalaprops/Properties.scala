package scalaprops

import scalaprops.internal._

final case class Properties[A] private[scalaprops] (props: Tree[(A, Option[Check])]) {
  def id: A = props.rootLabel._1

  private[this] def map[B](f: (A, Option[Check]) => (B, Option[Check])): Properties[B] =
    Properties(props.map(f.tupled))

  def andThenParam(f: Endo[Param]): Properties[A] =
    mapCheck(_.map(p => p.copy(paramEndo = p.paramEndo andThen f)))

  def andThenParamPF(f: PartialFunction[A, Endo[Param]]): Properties[A] =
    Properties(props.map {
      case (i, Some(m)) if f.isDefinedAt(i) =>
        i -> Some(Check(m.prop, m.paramEndo andThen f.apply(i)))
      case a => a
    })

  def composeParam(f: Endo[Param]): Properties[A] =
    mapCheck(_.map(p => p.copy(paramEndo = p.paramEndo compose f)))

  def mapId[B](f: A => B): Properties[B] =
    map((i, m) => (f(i), m))

  def widen[B](implicit e: A <:< B): Properties[B] =
    mapId[B](e)

  def mapRootId(f: A => A): Properties[A] =
    Properties(Tree.Node(f(props.rootLabel._1) -> props.rootLabel._2, props.subForest))

  def mapCheck(f: Option[Check] => Option[Check]): Properties[A] =
    map((i, m) => (i, f(m)))

  def ignore(reason: String): Properties[A] =
    mapCheck(_.map(_.ignore(reason)))

  def product[B](that: Properties[B]): Properties[Unit :-: A :-: B :-: Or.Empty] = {
    type T = Unit :-: A :-: B :-: Or.Empty
    Properties.noSort[T](
      Tree.Node(
        Or[T](()) -> Option.empty[Check],
        Stream(
          this.mapId(Or[T]._apply).props,
          that.mapId(Or[T]._apply).props
        )
      )
    )
  }
}

object Properties {
  def either[A, B](id: A, prop0: Properties[B], props: Properties[B]*): Properties[A :-: B :-: Or.Empty] = {
    type T = A :-: B :-: Or.Empty
    fromProps[T](
      Or[T](id),
      prop0.mapId(Or[T]._apply),
      props.map(_.mapId(Or[T]._apply))*
    )
  }

  def list[A](prop0: Properties[A], props: Properties[A]*): Properties[Unit :-: A :-: Or.Empty] = {
    either((), prop0, props*)
  }

  def single[A](id: A, c: Check): Properties[A] =
    Properties(Tree.Leaf(id -> Some(c)))

  def single[A](id: A, p: Property): Properties[A] =
    Properties(Tree.Leaf(id -> Some(p.toCheck)))

  private[this] def properties0[A](id: A, nodes: Stream[Tree[(A, Option[Check])]]): Properties[A] =
    Properties(
      distinctTree(
        Tree.Node(
          id -> Option.empty[Check],
          nodes
        )
      )
    )

  private[scalaprops] def noSort[A](tree: Tree[(A, Option[Check])]): Properties[A] =
    Properties(tree)

  def properties[A](id: A)(props: (A, Property)*): Properties[A] =
    properties0(
      id,
      props.map { case (n, p) => Tree.Leaf(n -> Option(p.toCheck)) }.toStream
    )

  def fromChecks[A](id: A)(checks: (A, Check)*): Properties[A] =
    properties0(
      id,
      checks.map { case (n, p) => Tree.Leaf(n -> Option(p)) }.toStream
    )

  def fromProps[A](id: A, prop0: Properties[A], props: Properties[A]*): Properties[A] =
    properties0(
      id,
      prop0.props #:: props.map(_.props).toStream
    )

  private[this] def distinctTree[A](tree: Tree[A]): Tree[A] = {
    val x = tree
      .mapAccumL(Set.empty[A]) { (set, a) =>
        if set.contains(a) then {
          (set, Option.empty[A])
        } else {
          (set + a, Some(a))
        }
      }
      ._2

    def loop(t: Tree[Option[A]]): Option[Tree[A]] =
      t.rootLabel.map { root => Tree.Node(root, t.subForest.flatMap(loop)) }

    loop(x).get
  }
}
