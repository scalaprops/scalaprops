package scalaprops

private[scalaprops] final case class TestResult(
  name: String,
  duration: Long,
  maxSize: Int,
  minSuccessful: Int
) {
  def asSimpleString: String = s"$duration $name $maxSize $minSuccessful"
}

private[scalaprops] object TestResult {

  def formatResults(results: collection.Seq[TestResult], count: Int): String = {
    results.sortBy(_.duration)(
      implicitly[scala.Ordering[Long]].reverse
    ).iterator.take(count).map(_.asSimpleString).mkString("\n")
  }

}
