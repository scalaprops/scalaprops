package scalaprops
import scalaprops.internal.Tree

final class ScalapropsTest {
  private[this] val tests1: MapType.Type[String, Property] = MapType.empty
  private[this] val tests2: MapType.Type[String, Properties[Any]] = MapType.empty

  private[scalaprops] def getAll(): List[Properties[Any]] =
    tests1.map { case (name, p) =>
      p.toProperties[Any](name)
    }.toList ::: tests2.map { case (name, p) =>
      Properties.noSort[Any](
        Tree.Node[(Any, Option[Check])](
          name -> Option.empty,
          p.props #:: Stream.empty
        )
      )
    }.toList

  def update(testName: String, property: Property): Unit =
    tests1.update(testName, property)

  def update[A](testName: String, property: Properties[A]): Unit =
    tests2.update(testName, property.asInstanceOf[Properties[Any]])
}
