package scalaprops

import scalaprops.Property.forAll
import scalaz._

object StringTest extends Scalaprops {

  val num = forAll{ str: (String @@ GenTags.Num) =>
    val expect = ('0' to '9').toSet
    Tag.unwrap(str).forall(expect)
  }

  val upper = forAll{ str: (String @@ GenTags.AlphaUpper) =>
    val expect = ('A' to 'Z').toSet
    Tag.unwrap(str).forall(expect)
  }

  val lower = forAll{ str: (String @@ GenTags.AlphaLower) =>
    val expect = ('a' to 'z').toSet
    Tag.unwrap(str).forall(expect)
  }

  val alpha = forAll{ str: (String @@ GenTags.Alpha) =>
    val expect = (('a' to 'z') ++ ('A' to 'Z')).toSet
    Tag.unwrap(str).forall(expect)
  }

  val alphaNum = forAll{ str: (String @@ GenTags.AlphaNum) =>
    val expect = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet
    Tag.unwrap(str).forall(expect)
  }

}
