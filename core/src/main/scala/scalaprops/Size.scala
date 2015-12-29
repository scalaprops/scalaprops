package scalaprops

import scalaz.{ICons, IList, NonEmptyList, Equal}

sealed abstract class Size extends Product with Serializable {
  def fold[A](range: (Int, Int) => A, frequency: Map[Int, Int] => A): A =
    this match {
      case Size.Range(min, max) =>
        range(min, max)
      case Size.Frequency(values) =>
        frequency(values)
    }
  def range: Option[(Int, Int)] =
    this match {
      case Size.Range(min, max) =>
        Some((min, max))
      case Size.Frequency(values) =>
        None
    }
  def frequency: Option[Map[Int, Int]] =
    this match {
      case Size.Range(min, max) =>
        None
      case Size.Frequency(values) =>
        Some(values)
    }
}

object Size {
  val default: Size = Range(0, 100)

  final case class Range(min: Int, max: Int) extends Size
  final case class Frequency(values: Map[Int, Int]) extends Size {
    lazy val gen: Gen[Int] = {
      val ICons(h, t) = values.foldRight(IList.empty[(Int, Gen[Int])]){
        case ((size, freq), acc) =>
          ICons((freq, Gen.value(size)), acc)
      }
      Gen.frequency(NonEmptyList.nel(h, t))
    }
  }

  implicit val sizeInstance: Equal[Size] =
    Equal.equal[Size]{
      case (Range(x1, y1), Range(x2, y2)) =>
        x1 == x2 && y1 == y2
      case (Frequency(a), Frequency(b)) =>
        a == b
      case _ =>
        false
    }

}
