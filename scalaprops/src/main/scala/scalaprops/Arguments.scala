package scalaprops

import scalaz.NonEmptyList

final case class Arguments(
  only: Option[NonEmptyList[String]],
  showDuration: Int
)

object Arguments {

  def parse(args: List[String]): Arguments = {
    val only = scalaz.std.list.toNel(
      args.dropWhile("--only" != _).drop(1).takeWhile(arg => !arg.startsWith("--"))
    )
    val showDuration = PartialFunction.condOpt(
      args.dropWhile("--showDuration" != _).drop(1).headOption
    ){
      case Some(UInt(n)) => n
    }.getOrElse(20)
    Arguments(
      only = only,
      showDuration = showDuration
    )
  }

  private[this] object UInt {
    def unapply(string: String): Option[Int] =
      try {
        val n = Integer.parseInt(string)
        if(n >= 0) Some(n)
        else None
      } catch {
        case _: NumberFormatException =>
          None
      }
  }
}
