package scalaprops

abstract class Choose[A] {
  def withBoundaries(from: A, to: A): Gen[A]
  def choose(from: A, to: A): Gen[A]
}

object Choose {
  def apply[A](implicit A: Choose[A]): Choose[A] = A

  implicit val intChoose: Choose[Int] =
    new Choose[Int] {
      override def withBoundaries(from: Int, to: Int) = {
        if (from == to) {
          Gen.value(from)
        } else {
          val min = math.min(from, to)
          val max = math.max(from, to)
          (max - min) match {
            case 1 =>
              Gen.elements(min, max)
            case 2 =>
              Gen.elements(min, min + 1, max)
            case _ =>
              Gen.frequency(
                1 -> Gen.value(min),
                1 -> Gen.value(min + 1),
                1 -> Gen.value(max - 1),
                1 -> Gen.value(max),
                90 -> Gen.choose(from, to)
              )
          }
        }
      }

      override def choose(from: Int, to: Int) =
        Gen.choose(from, to)
    }

  implicit val byteChoose: Choose[Byte] =
    new Choose[Byte] {
      override def withBoundaries(from: Byte, to: Byte) =
        Choose[Int].withBoundaries(from, to).map(Gen.Int2Byte)

      override def choose(from: Byte, to: Byte) =
        Choose[Int].choose(from, to).map(Gen.Int2Byte)
    }

  implicit val shortChoose: Choose[Short] =
    new Choose[Short] {
      override def withBoundaries(from: Short, to: Short) =
        Choose[Int].withBoundaries(from, to).map(Gen.Int2Short)

      override def choose(from: Short, to: Short) =
        Choose[Int].choose(from, to).map(Gen.Int2Short)
    }

  implicit val longChoose: Choose[Long] =
    new Choose[Long] {
      override def withBoundaries(from: Long, to: Long) = {
        if (from == to) {
          Gen.value(from)
        } else {
          val min = math.min(from, to)
          val max = math.max(from, to)
          (max - min) match {
            case 1 =>
              Gen.elements(min, max)
            case 2 =>
              Gen.elements(min, min + 1l, max)
            case _ =>
              Gen.frequency(
                1 -> Gen.value(min),
                1 -> Gen.value(min + 1l),
                1 -> Gen.value(max - 1l),
                1 -> Gen.value(max),
                90 -> Gen.chooseLong(from, to)
              )
          }
        }
      }

      override def choose(from: Long, to: Long) =
        Gen.chooseLong(from, to)
    }
}
