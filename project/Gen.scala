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

      val tupleN = {
        if(2 <= i && i <= 5)
          s"""Apply[Gen].tuple$i(${as.mkString(", ")})"""
        else if(i <= 12)
          s"""Apply[Gen].apply$i(${as.mkString(", ")})(Tuple$i.apply)"""
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
      ${((1 to d).map("t1._" + _) ++ t.zipWithIndex.tail.flatMap{case (x, n) => (1 to c).map("t" + (n + 1) + "._" + _)}).mkString("(",", ",")")}
    )"""
        }
      }

s"""  implicit final def f$i[$t, $Z](implicit ${as.map(a => s"$a: Cogen[$a]").mkString(", ")}, $Z: Gen[$Z]): Gen[$f] =
    ${as.foldRight(Z)((a, s) => s"f1($a, $s)")}.map(f => (${aa.mkString(", ")}) => f(${aa.mkString(")(")}))

  implicit final def tuple$i[$t](implicit ${as.map(a => s"$a: Gen[$a]").mkString(", ")}): Gen[Tuple$i[$t]] =
    $tupleN
"""
    }

s"""package scalaprops

import scalaz._

abstract class GenInstances private[scalaprops] {

  implicit final def f0[Z](implicit Z: Gen[Z]): Gen[Function0[Z]] =
    Z.map(z => () => z)

  implicit final def f1[A1, Z](implicit A1: Cogen[A1], Z: Gen[Z]): Gen[A1 => Z] =
    Gen.promote(x => A1.cogen(x, Z))

${(2 to 22).map(genF).mkString("\n")}
}
"""
  }
}
