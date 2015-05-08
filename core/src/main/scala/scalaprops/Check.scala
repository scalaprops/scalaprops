package scalaprops

import scalaz.Endo

final case class Check(prop: Property, paramEndo: Endo[Param] = Param.id) {
  def toProperties[A](id: A): Properties[A] =
    Properties.single(id, this)
  def ignore(reason: String): Check =
    copy(prop = prop.ignore(reason))
}
