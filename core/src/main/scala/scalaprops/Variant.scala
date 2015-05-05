package scalaprops

import java.util.WeakHashMap
import scalaz.Maybe

object Variant {

  private[this] final case class LongGen[A](n: Long, gen: Gen[A])

  private[this] val variantMemo: VariantCache[LongGen, Gen] =
    new VariantCache[LongGen, Gen]

  def variant[A](n: Long, g: Gen[A]): Gen[A] = {
    val p = new LongGen(n, g)
    variantMemo.get(p) match {
      case Maybe.Empty() =>
        val t = Gen.gen((i, r) => g.f(i, r.reseed(n)))
        variantMemo.put(p, t)
        t
      case Maybe.Just(gx) =>
        gx
    }
  }

  private[this] final class VariantCache[K[_], V[_]](delegate: WeakHashMap[Any, Any] = new WeakHashMap[Any, Any]) {
    def put[A](k: K[A], v: V[A]): this.type = {
      delegate.put(k, v)
      this
    }

    def get[A](k: K[A]): Maybe[V[A]] =
      Maybe.fromNullable(delegate.get(k).asInstanceOf[V[A]])
  }

}
