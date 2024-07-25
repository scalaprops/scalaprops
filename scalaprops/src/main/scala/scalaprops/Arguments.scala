package scalaprops

final case class Arguments(
  only: List[String],
  showDuration: Int,
  param: ParamOpt
)

object Arguments {
  private[this] val keyPrefix = "--"

  def objects(args: List[String]): Set[String] = {
    args.takeWhile(!_.startsWith(keyPrefix)).toSet
  }

  def parse(args: List[String]): Arguments = {
    val only = args.dropWhile((keyPrefix + "only") != _).drop(1).takeWhile(arg => !arg.startsWith(keyPrefix))

    def arg(key: String): Seq[String] =
      args.withFilter(_.startsWith(keyPrefix + key + "=")).flatMap {
        _.split('=').toList match {
          case _ :: v => v
          case _ => Nil
        }
      }

    def intArg(key: String): Option[Int] =
      arg(key).flatMap(int).lastOption

    def uintArg(key: String): Option[Int] =
      arg(key).flatMap(uint).lastOption

    def longArg(key: String): Option[Long] =
      arg(key).flatMap(long).lastOption

    Arguments(
      only = only,
      showDuration = uintArg("showDuration").getOrElse(20),
      param = ParamOpt(
        seed = {
          val key = "seed"
          intArg(key).map(Seed.IntSeed) orElse longArg(key).map(Seed.LongSeed)
        },
        minSuccessful = uintArg("minSuccessful"),
        maxDiscarded = uintArg("maxDiscarded"),
        minSize = uintArg("minSize"),
        maxSize = uintArg("maxSize"),
        timeoutSeconds = uintArg("timeout")
      )
    )
  }

  private[this] def long(string: String): Option[Long] = {
    try {
      val n = java.lang.Long.parseLong(string)
      if n >= 0 then Some(n)
      else None
    } catch {
      case _: NumberFormatException =>
        None
    }
  }

  private[this] def int(string: String): Option[Int] = {
    try {
      Some(Integer.parseInt(string))
    } catch {
      case _: NumberFormatException =>
        None
    }
  }

  private[this] def uint(string: String): Option[Int] = {
    try {
      val n = Integer.parseInt(string)
      if n >= 0 then Some(n)
      else None
    } catch {
      case _: NumberFormatException =>
        None
    }
  }
}
