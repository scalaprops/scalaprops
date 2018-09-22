package scalaprops

import java.util.concurrent.TimeUnit
import scalaprops.internal._
import scala.concurrent.duration.Duration

final case class Param(
  seed: Seed,
  minSuccessful: Int = Platform.minSuccessful,
  maxDiscarded: Int = 500,
  minSize: Int = 0,
  maxSize: Int = Gen.defaultSize,
  timeout: Duration = Duration(30, TimeUnit.SECONDS)
) {
  def rand: Rand = seed.createRand
}

object Param {
  def withCurrentTimeSeed(): Param = Param(
    seed = Seed.LongSeed(System.nanoTime())
  )

  def rand(rand: Rand): Endo[Param] =
    Endo(_.copy(seed = Seed.RandSeed(rand)))

  def constantSeed(value: Int): Endo[Param] =
    Endo(_.copy(seed = Seed.IntSeed(value)))

  def minSuccessful(n: Int): Endo[Param] =
    Endo(_.copy(minSuccessful = n))

  def maxSize(n: Int): Endo[Param] =
    Endo(_.copy(maxSize = n))

  def timeout(n: Int, timeunit: TimeUnit): Endo[Param] =
    Endo(_.copy(timeout = Duration(n, timeunit)))

  val id: Endo[Param] =
    Endo.idEndo[Param]
}
