package scalaprops

abstract class CogenInstances1 {
  implicit def cogenLazylist[A: Cogen]: Cogen[LazyList[A]] =
    Cogen[List[A]].contramap(_.toList)
}
