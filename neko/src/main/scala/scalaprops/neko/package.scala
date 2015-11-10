package scalaprops

import cats.laws.IsEq

package object neko {

  type Eq[A] = cats.Eq[A]

  def forAllNeko[A1, B](f: A1 => IsEq[B])(implicit A1: Gen[A1], B: Eq[B]): Property =
    Property.forAll{ a1: A1 =>
      val x = f(a1)
      if(B.eqv(x.lhs, x.rhs)) true
      else sys.error(s"${x.lhs} != ${x.rhs}")
    }

  def forAllNeko[A1, A2, B](f: (A1, A2) => IsEq[B])(implicit A1: Gen[A1], A2: Gen[A2], B: Eq[B]): Property =
    Property.forAll{ (a1: A1, a2: A2) =>
      val x = f(a1, a2)
      if(B.eqv(x.lhs, x.rhs)) true
      else sys.error(s"${x.lhs} != ${x.rhs}")
    }

  def forAllNeko[A1, A2, A3, B](f: (A1, A2, A3) => IsEq[B])(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], B: Eq[B]): Property =
    Property.forAll{ (a1: A1, a2: A2, a3: A3) =>
      val x = f(a1, a2, a3)
      if(B.eqv(x.lhs, x.rhs)) true
      else sys.error(s"${x.lhs} != ${x.rhs}")
    }

  def forAllNeko[A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => IsEq[B])(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], B: Eq[B]): Property =
    Property.forAll{ (a1: A1, a2: A2, a3: A3, a4: A4) =>
      val x = f(a1, a2, a3, a4)
      if(B.eqv(x.lhs, x.rhs)) true
      else sys.error(s"${x.lhs} != ${x.rhs}")
    }

  def forAllNeko[A1, A2, A3, A4, A5, B](f: (A1, A2, A3, A4, A5) => IsEq[B])(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], B: Eq[B]): Property =
    Property.forAll{ (a1: A1, a2: A2, a3: A3, a4: A4, a5: A5) =>
      val x = f(a1, a2, a3, a4, a5)
      if(B.eqv(x.lhs, x.rhs)) true
      else sys.error(s"${x.lhs} != ${x.rhs}")
    }

}
