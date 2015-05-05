object Cogen {
  def gen: String = {
    val A = "A"
    val Z = "Z"
    val C = "Cogen"
    val X = "X"

    val cogen = (i: Int) => {
      val as = (1 to i).map(A + _)
      val t = as.mkString(", ")
      val f = s"($t) => $Z"
      val tp = s"Tuple$i[$t]"
      val tpeF = s"$C[$f]"
      val tpeT = s"$C[$tp]"

      val function =
s"""  implicit final def f$i[$t, $Z](implicit ${as.map(a => s"$a: Gen[$a]").mkString(", ")}, $Z: $C[$Z]): $tpeF =
    new $tpeF {
      def cogen[$X](f: $f, g: Gen[$X]) =
        ${as.foldRight(Z)((a, s) => s"f1($a, $s)") + ".cogen(f.curried, g)"}
    }
"""

      val tuple =
s"""
  implicit final def tuple$i[$t, $Z](implicit ${as.map(a => s"$a: Cogen[$a]").mkString(", ")}): $tpeT =
    new $tpeT {
      def cogen[$X](t: $tp, g: Gen[$X]) =
        ${as.zipWithIndex.foldRight("g"){case ((a, m), s) => s"$a.cogen(t._${m + 1}, $s)"}}
    }
"""
      if(i == 1) {
        tuple
      } else {
        function + tuple
      }
    }

s"""package scalaprops

abstract class CogenInstances private[scalaprops] {

  implicit final def f1[A1, Z](implicit A1: Gen[A1], C: Cogen[Z]): Cogen[A1 => Z] =
    new Cogen[A1 => Z] {
      def cogen[X](f: A1 => Z, g: Gen[X]) =
        A1.flatMap(x => C.cogen(f(x), g))
    }

${(1 to 22).map(cogen).mkString("\n")}
}
"""

  }
}
