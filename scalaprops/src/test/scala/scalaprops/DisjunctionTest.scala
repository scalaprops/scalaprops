package scalaprops

import scalaz._
import scalaz.std.anyVal._

object DisjunctionTest extends Scalaprops {

  val bitraverse = scalazlaws.bitraverse.all[\/]
  val associative = scalazlaws.associative.all[\/]
  val order = scalazlaws.order.all[Int \/ Int]
  val monoid = scalazlaws.monoid.all[IList[Byte] \/ IList[Byte]]

  val laws1 = Properties.list(
    scalazlaws.monadError.all[({type l[a] = Int \/ a})#l, Int],
    scalazlaws.traverse.all[({type l[a] = Int \/ a})#l],
    scalazlaws.bindRec.all[({type l[a] = Int \/ a})#l],
    scalazlaws.plus.all[({type l[a] = Int \/ a})#l]
  )

  val anotherPlus = {
    implicit def disjunctionPlus[L](implicit L: Semigroup[L]): Plus[({type l[a] = L Either a})#l] =
      new Plus[({type l[a] = L Either a})#l] {
        def plus[A](x: L Either A, y: => L Either A) = x match {
          case Left(lx) => y match {
            case Left(ly) => Left(L.append(lx, ly))
            case r @ Right(_) => r
          }
          case _ => x
        }
      }
    import scalaz.std.either.eitherEqual
    scalazlaws.plus.all[({type l[a] = IList[Int] Either a})#l]
  }
}
