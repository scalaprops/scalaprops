package scalaprops

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

final case class ParamOpt(
  seed: Option[Seed],
  minSuccessful: Option[Int],
  maxDiscarded: Option[Int],
  minSize: Option[Int],
  maxSize: Option[Int],
  timeoutSeconds: Option[Int]
) {
  def merge(param: Param): Param = {
    Param(
      seed = seed.getOrElse(param.seed),
      minSuccessful = minSuccessful.getOrElse(param.minSuccessful),
      maxDiscarded = maxDiscarded.getOrElse(param.maxDiscarded),
      minSize = minSize.getOrElse(param.minSize),
      maxSize = maxSize.getOrElse(param.maxSize),
      timeout = timeoutSeconds match {
        case Some(n) => Duration(n, TimeUnit.SECONDS)
        case None => param.timeout
      }
    )
  }
}
