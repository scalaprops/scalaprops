package scalaprops

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scalaz.Endo

final case class Param(
  rand: Rand,
  minSuccessful: Int = Platform.minSuccessful,
  maxDiscarded: Int = 500,
  minSize: Int = 0,
  maxSize: Int = Gen.defaultSize,
  timeout: Duration = Duration(30, TimeUnit.SECONDS)
)

object Param {
  def withCurrentTimeSeed(): Param = Param(
    rand = Platform.randFromLong(System.nanoTime())
  )

  def rand(rand: Rand): Endo[Param] =
    Endo(_.copy(rand = rand))

  def constantSeed(value: Int): Endo[Param] =
    Endo(_.copy(rand = Rand.fromSeed(value)))

  def minSuccessful(n: Int): Endo[Param] =
    Endo(_.copy(minSuccessful = n))

  def maxSize(n: Int): Endo[Param] =
    Endo(_.copy(maxSize = n))

  def timeout(n: Int, timeunit: TimeUnit): Endo[Param] =
    Endo(_.copy(timeout = Duration(n, timeunit)))

  val id: Endo[Param] =
    Endo.idEndo[Param]
}
