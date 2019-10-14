package scalaprops

sealed abstract class Result extends Product with Serializable {
  def isUnfalsified = this.isInstanceOf[Result.Unfalsified]
  def isFalsified = this.isInstanceOf[Result.Falsified]
  def isProven = this eq Result.Proven
  def isException = this.isInstanceOf[Result.Exception]
  def isIgnored = this.isInstanceOf[Result.Ignored]
  def isNoResult = this.isInstanceOf[Result.NoResult.type]

  final def toOption: Option[HasResult] = this match {
    case r: HasResult => Some(r)
    case Result.NoResult => None
  }

  final def failed: Boolean = isFalsified || isException
}

sealed abstract class HasResult extends Result {
  final def provenAsUnfalsified: AddArgs = this match {
    case Result.Proven =>
      Result.Unfalsified(List.empty)
    case a: AddArgs =>
      a
  }
}

sealed abstract class AddArgs extends HasResult {
  def addArg(a: Arg): AddArgs
}

object Result {
  final case class Unfalsified(args: List[Arg]) extends AddArgs {
    override def addArg(a: Arg): AddArgs = copy(a :: args)
  }
  final case class Falsified(args: List[Arg]) extends AddArgs {
    override def addArg(a: Arg): AddArgs = copy(a :: args)
  }
  case object Proven extends HasResult
  final case class Exception(args: List[Arg], exception: Throwable) extends AddArgs {
    override def addArg(a: Arg): AddArgs = copy(a :: args)
  }
  final case class Ignored(reason: String) extends AddArgs { self =>
    override def addArg(a: Arg): AddArgs = self
  }
  case object NoResult extends Result
}
