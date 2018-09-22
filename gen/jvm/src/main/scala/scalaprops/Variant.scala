package scalaprops

import java.util.WeakHashMap

object Variant {

  private[this] final case class LongGen[A](n: Long, state: CogenState[A])

  private[this] val variantMemo: VariantCache[LongGen, CogenState] =
    new VariantCache[LongGen, CogenState]

  def variantInt[A](n: Int, g: CogenState[A]): CogenState[A] =
    variant(n, g)

  def variant[A](n: Long, g: CogenState[A]): CogenState[A] = {
    val (next, int) = g.rand.nextInt
    val seed = n + int
    val p = new LongGen(seed, g)
    variantMemo.get(p) match {
      case Some(gx) =>
        gx
      case _ =>
        val t = CogenState(next, Gen.gen((i, r) => g.gen.f(i, r.reseed(seed))))
        variantMemo.put(p, t)
        t
    }
  }

  private[this] final class VariantCache[K[_], V[_]](delegate: WeakHashMap[Any, Any] = new WeakHashMap[Any, Any]) {
    def put[A](k: K[A], v: V[A]): this.type = {
      delegate.put(k, v)
      this
    }

    def get[A](k: K[A]): Option[V[A]] =
      Option(delegate.get(k).asInstanceOf[V[A]])
  }

}
