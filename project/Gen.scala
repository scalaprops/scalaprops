object Gen {
  def gen: String = {
    val A = "A"
    val Z = "Z"
    val G = "Gen"
    val X = "X"

    val genF = (i: Int) => {
      val as = (1 to i).map(A + _)
      val aa = (1 to i).map("a" + _)
      val t = as.mkString(", ")
      val f = s"($t) => $Z"
      val ff = "f"

      val applyN = {
        if(i <= 12)
          s"""Apply[Gen].apply$i(${as.mkString(", ")})($ff)"""
        else {
          val a = math.ceil(i / 12.0).toInt
          val c = math.ceil(i / a.toDouble).toInt
          val d = i - ((a - 1) * c)
          def e(start: Int, end: Int) = s"""tuple$c(${(start to end).map(i => A + i).mkString(", ")})"""
          val t = (1 to a).map("t" + _)
          s"""Apply[Gen].apply$a(
      tuple$d(${as.take(d).mkString(", ")}),
      ${(2 to a).map(x => e((x - 2) * c + d + 1, (x - 1) * c + d)).mkString(", ")}
    )((${t.mkString(", ")}) =>
      $ff${((1 to d).map("t1._" + _) ++ t.zipWithIndex.tail.flatMap{case (x, n) => (1 to c).map("t" + (n + 1) + "._" + _)}).mkString("(",", ",")")}
    )"""
        }
      }

      def from(name: String) = s"final def $name[$t, $Z]($ff: ($t) => $Z)(implicit ${as.map(a => s"$a: Gen[$a]").mkString(", ")}): Gen[$Z] ="

s"""  implicit final def f$i[$t, $Z](implicit ${as.map(a => s"$a: Cogen[$a]").mkString(", ")}, $Z: Gen[$Z]): Gen[$f] =
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

import scalaz._
import Gen.f1

abstract class GenInstances private[scalaprops] {

${(2 to 22).map(genF).mkString("\n")}
}
"""
  }
}
