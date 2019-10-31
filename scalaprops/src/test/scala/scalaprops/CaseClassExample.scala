package scalaprops

object CaseClassExample {
  final case class UserId(value: Long)
  object UserId {
    implicit val gen: Gen[UserId] = Gen.from(apply _)
    implicit val cogen: Cogen[UserId] = Cogen.from(unapply _)
  }

  Gen[UserId]
  Cogen[UserId]

  final case class CaseClass2(a: Long, b: Boolean)
  object CaseClass2 {
    implicit val gen: Gen[CaseClass2] = Gen.from(apply _)
    implicit val cogen: Cogen[CaseClass2] = Cogen.from2(unapply _)
  }

  Gen[CaseClass2]
  Cogen[CaseClass2]
}
