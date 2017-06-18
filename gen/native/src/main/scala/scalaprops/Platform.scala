package scalaprops

private[scalaprops] object Platform {
  def minSuccessful = 100
  def randFromLong(seed: Long) = MersenneTwister32.standard(seed)
  def randFromInt(seed: Int) = MersenneTwister32.standard(seed)
  def genSize = 100
  def className[A](clazz: Class[A]) = clazz.getSimpleName
}
