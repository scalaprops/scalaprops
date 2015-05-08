package scalaprops

import scalaz.{IList, Maybe}

sealed abstract class Result extends Product with Serializable {
  def isUnfalsified = this.isInstanceOf[Result.Unfalsified]
  def isFalsified = this.isInstanceOf[Result.Falsified]
  def isProven = this.isInstanceOf[Result.Proven]
  def isException = this.isInstanceOf[Result.Exception]
  def isIgnored = this.isInstanceOf[Result.Ignored]
  def isNoResult = this.isInstanceOf[Result.NoResult.type]

  def toMaybe: Maybe[Result] =
    if(isNoResult) Maybe.Empty() else Maybe.just(this)

  def provenAsUnfalsified: Result = this match {
    case Result.Proven(args) =>
      Result.Unfalsified(args)
    case _ =>
      this
  }

  def addArg(a: Arg): Result

  def failed: Boolean = isFalsified || isException
}

object Result {
  final case class Unfalsified(args: IList[Arg]) extends Result {
    override def addArg(a: Arg): Result = copy(a :: args)
  }
  final case class Falsified(args: IList[Arg]) extends Result {
    override def addArg(a: Arg): Result = copy(a :: args)
  }
  final case class Proven(args: IList[Arg]) extends Result {
    override def addArg(a: Arg): Result = copy(a :: args)
  }
  final case class Exception(args: IList[Arg], exception: Throwable) extends Result {
    override def addArg(a: Arg): Result = copy(a :: args)
  }
  final case class Ignored(reason: String) extends Result { self =>
    override def addArg(a: Arg): Result = self
  }
  case object NoResult extends Result {
    override def addArg(a: Arg): Result = this // TODO error ?
  }

}
