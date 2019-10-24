package scalaprops

abstract class GenInstances2 {
  implicit def lazylistGen[A](implicit A: Gen[A]): Gen[LazyList[A]] =
    Gen.listOf_[LazyList, A](A, 0, _.to(LazyList))
}
