package scalaprops

object Variant {
  def variant[A](n: Long, g: CogenState[A]): CogenState[A] = {
    val (next, int) = g.rand.nextInt
    val seed = n + int
    CogenState(next, Gen.gen((i, r) => g.gen.f(i, r.reseed(seed))))
  }
  def variantInt[A](n: Int, g: CogenState[A]): CogenState[A] = {
    val (next, int) = g.rand.nextInt
    val seed = n + int
    CogenState(next, Gen.gen((i, r) => g.gen.f(i, r.setIntSeed(seed))))
  }
}
