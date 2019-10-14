object Gen {
  def gen: String = {
    val A = "A"
    val Z = "Z"

    val genF = (i: Int) => {
      val as = (1 to i).map(A + _)
      val aa = (1 to i).map("a" + _)
      val t = as.mkString(", ")
      val f = s"($t) => $Z"
      val ff = "f"

      val applyN = {
        (aa, as).zipped.map { _ + " <- " + _ }.mkString("for { ", " ; ", s" } yield $ff(${aa.mkString(", ")})")
      }

      def from(name: String) =
        s"final def $name[$t, $Z]($ff: ($t) => $Z)(implicit ${as.map(a => s"$a: Gen[$a]").mkString(", ")}): Gen[$Z] ="

      s"""  implicit final def f$i[$t, $Z](implicit ${as
        .map(a => s"$a: Cogen[$a]")
        .mkString(", ")}, $Z: Gen[$Z]): Gen[$f] =
    ${as.foldRight(Z)((a, s) => s"f1($a, $s)")}.map(f => (${aa.mkString(", ")}) => f(${aa.mkString(")(")}))

  implicit final def tuple$i[$t](implicit ${as.map(a => s"$a: Gen[$a]").mkString(", ")}): Gen[Tuple$i[$t]] =
    from$i[$t, Tuple$i[$t]](Tuple$i.apply)($t)

  ${from("from")}
    from$i($ff)($t)

  ${from("from" + i)}
    $applyN
"""
    }

    s"""package scalaprops

import Gen.f1

abstract class GenInstances private[scalaprops] {

${(2 to 22).map(genF).mkString("\n")}
}
"""
  }
}
