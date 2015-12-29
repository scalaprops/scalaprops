package scalaprops

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scalaz.Endo

final case class Param(
  rand: Rand,
  minSuccessful: Int = 100,
  maxDiscarded: Int = 500,
  size: Size = Size.default,
  timeout: Duration = Duration(30, TimeUnit.SECONDS)
) {

  def setMaxSize(n: Int): Param =
    copy(
      size = size.range.fold(Size.Range(0, n)){
        case (min, _) => Size.Range(min, n)
      }
    )
}

object Param {
  def withCurrentTimeSeed(): Param = Param(
    rand = Rand.standard(System.nanoTime()),
    size = Size.Frequency(
      (0 to 100).map(_ -> 1).toMap
    )
  )

  def rand(rand: Rand): Endo[Param] =
    Endo(_.copy(rand = rand))

  def constantSeed(value: Int): Endo[Param] =
    Endo(_.copy(rand = Rand.fromSeed(value)))

  def minSuccessful(n: Int): Endo[Param] =
    Endo(_.copy(minSuccessful = n))

  def maxSize(n: Int): Endo[Param] =
    Endo(_.setMaxSize(n))

  def size(s: Size): Endo[Param] =
    Endo(_.copy(size = s))

  def timeout(n: Int, timeunit: TimeUnit): Endo[Param] =
    Endo(_.copy(timeout = Duration(n, timeunit)))

  val id: Endo[Param] =
    Endo.idEndo[Param]
}
