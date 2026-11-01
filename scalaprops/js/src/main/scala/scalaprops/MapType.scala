package scalaprops

private[scalaprops] object MapType {
  type Type[A, B] = scala.collection.mutable.Map[A, B]
  def empty[A, B]: Type[A, B] = scala.collection.mutable.Map.empty[A, B]
}
