package scalaprops

import scala.annotation.tailrec
import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.deriving.Mirror

sealed abstract class AutoDerivingInstances { self: AutoDeriving.type =>

  inline implicit def genProduct[A](using mirror: Mirror.ProductOf[A]): Gen[A] = {
    val xs = deriveGenRec[mirror.MirroredElemTypes]
    val n = xs.length
    Gen.gen[A]{(size, rand) =>
      if (n > 0) {
        val values = new Array[AnyRef](n)

        @tailrec
        def loop(acc: Rand, i: Int): (Rand, A) = {
          if (i < n) {
            val (nextR, result) = xs(i).f(size, acc)
            values(i) = result.asInstanceOf[AnyRef]
            loop(nextR, i + 1)
          } else {
            (acc, mirror.fromProduct(
              new Product {
                override def canEqual(that: Any): Boolean = true
                override def productArity = n
                override def productElement(i: Int): Any = values(i)
              }
            ))
          }
        }
        loop(rand, 0)
      } else {
        (rand, mirror.fromProduct(None))
      }
    }
  }

  inline def deriveGenRec[T <: Tuple]: List[Gen[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple =>
        Nil
      case _: (t *: ts) =>
        deriveGen[t] :: deriveGenRec[ts]
    }

  inline implicit def genSum[A](using mirror: Mirror.SumOf[A]): Gen[A] = {
    val xs = deriveGenRec[mirror.MirroredElemTypes]
    Gen.gen[A]{(size, r1) =>
      val (r2, i) = r1.nextInt
      val index = if (i == Int.MinValue) {
        0
      } else {
        i.abs % xs.length
      }
      xs(index).f(size, r2).asInstanceOf[(Rand, A)]
    }
  }

}

object AutoDeriving extends AutoDerivingInstances {

  inline implicit def deriveGen[A]: Gen[A] =
    summonFrom {
      case a: Gen[A]  =>
        a
      case a: Mirror.ProductOf[A]  =>
        genProduct[A]
      case a: Mirror.SumOf[A]  =>
        genSum[A]
    }

}
