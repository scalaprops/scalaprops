package scalaprops

abstract class CogenInstances private[scalaprops] {

  import Cogen.f1


  implicit final def tuple1[A1](implicit A1: Cogen[A1]): Cogen[Tuple1[A1]] =
    new Cogen[Tuple1[A1]] {
      def cogen[X](t: Tuple1[A1], g: CogenState[X]) =
        A1.cogen(t._1, g)
    }

  implicit final def f2[A1, A2, Z](implicit A1: Gen[A1], A2: Gen[A2], Z: Cogen[Z]): Cogen[(A1, A2) => Z] =
    new Cogen[(A1, A2) => Z] {
      def cogen[X](f: (A1, A2) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, Z)).cogen(f.curried, g)
    }

  implicit final def tuple2[A1, A2](implicit A1: Cogen[A1], A2: Cogen[A2]): Cogen[Tuple2[A1, A2]] =
    new Cogen[Tuple2[A1, A2]] {
      def cogen[X](t: Tuple2[A1, A2], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, g))
    }

  final def from[A1, A2, Z](f: Z => Option[Tuple2[A1, A2]])(implicit A1: Cogen[A1], A2: Cogen[A2]): Cogen[Z] =
    from2[A1, A2, Z](f)(A1, A2)

  final def from2[A1, A2, Z](f: Z => Option[Tuple2[A1, A2]])(implicit A1: Cogen[A1], A2: Cogen[A2]): Cogen[Z] =
    tuple2[A1, A2](A1, A2).contramap(t => f(t).get)

  implicit final def f3[A1, A2, A3, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], Z: Cogen[Z]): Cogen[(A1, A2, A3) => Z] =
    new Cogen[(A1, A2, A3) => Z] {
      def cogen[X](f: (A1, A2, A3) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, Z))).cogen(f.curried, g)
    }

  implicit final def tuple3[A1, A2, A3](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3]): Cogen[Tuple3[A1, A2, A3]] =
    new Cogen[Tuple3[A1, A2, A3]] {
      def cogen[X](t: Tuple3[A1, A2, A3], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, g)))
    }

  final def from[A1, A2, A3, Z](f: Z => Option[Tuple3[A1, A2, A3]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3]): Cogen[Z] =
    from3[A1, A2, A3, Z](f)(A1, A2, A3)

  final def from3[A1, A2, A3, Z](f: Z => Option[Tuple3[A1, A2, A3]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3]): Cogen[Z] =
    tuple3[A1, A2, A3](A1, A2, A3).contramap(t => f(t).get)

  implicit final def f4[A1, A2, A3, A4, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4) => Z] =
    new Cogen[(A1, A2, A3, A4) => Z] {
      def cogen[X](f: (A1, A2, A3, A4) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, Z)))).cogen(f.curried, g)
    }

  implicit final def tuple4[A1, A2, A3, A4](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4]): Cogen[Tuple4[A1, A2, A3, A4]] =
    new Cogen[Tuple4[A1, A2, A3, A4]] {
      def cogen[X](t: Tuple4[A1, A2, A3, A4], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, g))))
    }

  final def from[A1, A2, A3, A4, Z](f: Z => Option[Tuple4[A1, A2, A3, A4]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4]): Cogen[Z] =
    from4[A1, A2, A3, A4, Z](f)(A1, A2, A3, A4)

  final def from4[A1, A2, A3, A4, Z](f: Z => Option[Tuple4[A1, A2, A3, A4]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4]): Cogen[Z] =
    tuple4[A1, A2, A3, A4](A1, A2, A3, A4).contramap(t => f(t).get)

  implicit final def f5[A1, A2, A3, A4, A5, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5) => Z] =
    new Cogen[(A1, A2, A3, A4, A5) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, Z))))).cogen(f.curried, g)
    }

  implicit final def tuple5[A1, A2, A3, A4, A5](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5]): Cogen[Tuple5[A1, A2, A3, A4, A5]] =
    new Cogen[Tuple5[A1, A2, A3, A4, A5]] {
      def cogen[X](t: Tuple5[A1, A2, A3, A4, A5], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, g)))))
    }

  final def from[A1, A2, A3, A4, A5, Z](f: Z => Option[Tuple5[A1, A2, A3, A4, A5]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5]): Cogen[Z] =
    from5[A1, A2, A3, A4, A5, Z](f)(A1, A2, A3, A4, A5)

  final def from5[A1, A2, A3, A4, A5, Z](f: Z => Option[Tuple5[A1, A2, A3, A4, A5]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5]): Cogen[Z] =
    tuple5[A1, A2, A3, A4, A5](A1, A2, A3, A4, A5).contramap(t => f(t).get)

  implicit final def f6[A1, A2, A3, A4, A5, A6, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, Z)))))).cogen(f.curried, g)
    }

  implicit final def tuple6[A1, A2, A3, A4, A5, A6](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6]): Cogen[Tuple6[A1, A2, A3, A4, A5, A6]] =
    new Cogen[Tuple6[A1, A2, A3, A4, A5, A6]] {
      def cogen[X](t: Tuple6[A1, A2, A3, A4, A5, A6], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, g))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, Z](f: Z => Option[Tuple6[A1, A2, A3, A4, A5, A6]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6]): Cogen[Z] =
    from6[A1, A2, A3, A4, A5, A6, Z](f)(A1, A2, A3, A4, A5, A6)

  final def from6[A1, A2, A3, A4, A5, A6, Z](f: Z => Option[Tuple6[A1, A2, A3, A4, A5, A6]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6]): Cogen[Z] =
    tuple6[A1, A2, A3, A4, A5, A6](A1, A2, A3, A4, A5, A6).contramap(t => f(t).get)

  implicit final def f7[A1, A2, A3, A4, A5, A6, A7, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, Z))))))).cogen(f.curried, g)
    }

  implicit final def tuple7[A1, A2, A3, A4, A5, A6, A7](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7]): Cogen[Tuple7[A1, A2, A3, A4, A5, A6, A7]] =
    new Cogen[Tuple7[A1, A2, A3, A4, A5, A6, A7]] {
      def cogen[X](t: Tuple7[A1, A2, A3, A4, A5, A6, A7], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, g)))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, Z](f: Z => Option[Tuple7[A1, A2, A3, A4, A5, A6, A7]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7]): Cogen[Z] =
    from7[A1, A2, A3, A4, A5, A6, A7, Z](f)(A1, A2, A3, A4, A5, A6, A7)

  final def from7[A1, A2, A3, A4, A5, A6, A7, Z](f: Z => Option[Tuple7[A1, A2, A3, A4, A5, A6, A7]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7]): Cogen[Z] =
    tuple7[A1, A2, A3, A4, A5, A6, A7](A1, A2, A3, A4, A5, A6, A7).contramap(t => f(t).get)

  implicit final def f8[A1, A2, A3, A4, A5, A6, A7, A8, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, Z)))))))).cogen(f.curried, g)
    }

  implicit final def tuple8[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8]): Cogen[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] =
    new Cogen[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] {
      def cogen[X](t: Tuple8[A1, A2, A3, A4, A5, A6, A7, A8], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, g))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, Z](f: Z => Option[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8]): Cogen[Z] =
    from8[A1, A2, A3, A4, A5, A6, A7, A8, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8)

  final def from8[A1, A2, A3, A4, A5, A6, A7, A8, Z](f: Z => Option[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8]): Cogen[Z] =
    tuple8[A1, A2, A3, A4, A5, A6, A7, A8](A1, A2, A3, A4, A5, A6, A7, A8).contramap(t => f(t).get)

  implicit final def f9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, Z))))))))).cogen(f.curried, g)
    }

  implicit final def tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9]): Cogen[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    new Cogen[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] {
      def cogen[X](t: Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, g)))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](f: Z => Option[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9]): Cogen[Z] =
    from9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9)

  final def from9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](f: Z => Option[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9]): Cogen[Z] =
    tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9](A1, A2, A3, A4, A5, A6, A7, A8, A9).contramap(t => f(t).get)

  implicit final def f10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, Z)))))))))).cogen(f.curried, g)
    }

  implicit final def tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10]): Cogen[Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    new Cogen[Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] {
      def cogen[X](t: Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, g))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](f: Z => Option[Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10]): Cogen[Z] =
    from10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)

  final def from10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](f: Z => Option[Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10]): Cogen[Z] =
    tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10).contramap(t => f(t).get)

  implicit final def f11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, Z))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11]): Cogen[Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    new Cogen[Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] {
      def cogen[X](t: Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, g)))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](f: Z => Option[Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11]): Cogen[Z] =
    from11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)

  final def from11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](f: Z => Option[Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11]): Cogen[Z] =
    tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11).contramap(t => f(t).get)

  implicit final def f12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, Z)))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12]): Cogen[Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    new Cogen[Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] {
      def cogen[X](t: Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, g))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](f: Z => Option[Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12]): Cogen[Z] =
    from12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)

  final def from12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](f: Z => Option[Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12]): Cogen[Z] =
    tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12).contramap(t => f(t).get)

  implicit final def f13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, Z))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13]): Cogen[Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    new Cogen[Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] {
      def cogen[X](t: Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, g)))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](f: Z => Option[Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13]): Cogen[Z] =
    from13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)

  final def from13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](f: Z => Option[Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13]): Cogen[Z] =
    tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13).contramap(t => f(t).get)

  implicit final def f14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, Z)))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14]): Cogen[Tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    new Cogen[Tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] {
      def cogen[X](t: Tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, g))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](f: Z => Option[Tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14]): Cogen[Z] =
    from14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)

  final def from14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](f: Z => Option[Tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14]): Cogen[Z] =
    tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14).contramap(t => f(t).get)

  implicit final def f15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, Z))))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15]): Cogen[Tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    new Cogen[Tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] {
      def cogen[X](t: Tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, A15.cogen(t._15, g)))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](f: Z => Option[Tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15]): Cogen[Z] =
    from15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)

  final def from15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](f: Z => Option[Tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15]): Cogen[Z] =
    tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15).contramap(t => f(t).get)

  implicit final def f16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, Z)))))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16]): Cogen[Tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    new Cogen[Tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] {
      def cogen[X](t: Tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, A15.cogen(t._15, A16.cogen(t._16, g))))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](f: Z => Option[Tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16]): Cogen[Z] =
    from16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)

  final def from16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](f: Z => Option[Tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16]): Cogen[Z] =
    tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16).contramap(t => f(t).get)

  implicit final def f17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, Z))))))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17]): Cogen[Tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    new Cogen[Tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] {
      def cogen[X](t: Tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, A15.cogen(t._15, A16.cogen(t._16, A17.cogen(t._17, g)))))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](f: Z => Option[Tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17]): Cogen[Z] =
    from17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)

  final def from17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](f: Z => Option[Tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17]): Cogen[Z] =
    tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17).contramap(t => f(t).get)

  implicit final def f18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, Z)))))))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18]): Cogen[Tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    new Cogen[Tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] {
      def cogen[X](t: Tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, A15.cogen(t._15, A16.cogen(t._16, A17.cogen(t._17, A18.cogen(t._18, g))))))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](f: Z => Option[Tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18]): Cogen[Z] =
    from18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)

  final def from18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](f: Z => Option[Tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18]): Cogen[Z] =
    tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18).contramap(t => f(t).get)

  implicit final def f19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, f1(A19, Z))))))))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19]): Cogen[Tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    new Cogen[Tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] {
      def cogen[X](t: Tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, A15.cogen(t._15, A16.cogen(t._16, A17.cogen(t._17, A18.cogen(t._18, A19.cogen(t._19, g)))))))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](f: Z => Option[Tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19]): Cogen[Z] =
    from19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)

  final def from19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](f: Z => Option[Tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19]): Cogen[Z] =
    tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19).contramap(t => f(t).get)

  implicit final def f20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, f1(A19, f1(A20, Z)))))))))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20]): Cogen[Tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    new Cogen[Tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] {
      def cogen[X](t: Tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, A15.cogen(t._15, A16.cogen(t._16, A17.cogen(t._17, A18.cogen(t._18, A19.cogen(t._19, A20.cogen(t._20, g))))))))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](f: Z => Option[Tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20]): Cogen[Z] =
    from20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)

  final def from20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](f: Z => Option[Tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20]): Cogen[Z] =
    tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20).contramap(t => f(t).get)

  implicit final def f21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], A21: Gen[A21], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, f1(A19, f1(A20, f1(A21, Z))))))))))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], A21: Cogen[A21]): Cogen[Tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    new Cogen[Tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] {
      def cogen[X](t: Tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, A15.cogen(t._15, A16.cogen(t._16, A17.cogen(t._17, A18.cogen(t._18, A19.cogen(t._19, A20.cogen(t._20, A21.cogen(t._21, g)))))))))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](f: Z => Option[Tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], A21: Cogen[A21]): Cogen[Z] =
    from21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)

  final def from21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](f: Z => Option[Tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], A21: Cogen[A21]): Cogen[Z] =
    tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21).contramap(t => f(t).get)

  implicit final def f22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], A21: Gen[A21], A22: Gen[A22], Z: Cogen[Z]): Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z] =
    new Cogen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z] {
      def cogen[X](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z, g: CogenState[X]) =
        f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, f1(A19, f1(A20, f1(A21, f1(A22, Z)))))))))))))))))))))).cogen(f.curried, g)
    }

  implicit final def tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], A21: Cogen[A21], A22: Cogen[A22]): Cogen[Tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    new Cogen[Tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] {
      def cogen[X](t: Tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22], g: CogenState[X]) =
        A1.cogen(t._1, A2.cogen(t._2, A3.cogen(t._3, A4.cogen(t._4, A5.cogen(t._5, A6.cogen(t._6, A7.cogen(t._7, A8.cogen(t._8, A9.cogen(t._9, A10.cogen(t._10, A11.cogen(t._11, A12.cogen(t._12, A13.cogen(t._13, A14.cogen(t._14, A15.cogen(t._15, A16.cogen(t._16, A17.cogen(t._17, A18.cogen(t._18, A19.cogen(t._19, A20.cogen(t._20, A21.cogen(t._21, A22.cogen(t._22, g))))))))))))))))))))))
    }

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](f: Z => Option[Tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], A21: Cogen[A21], A22: Cogen[A22]): Cogen[Z] =
    from22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)

  final def from22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](f: Z => Option[Tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]])(implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], A21: Cogen[A21], A22: Cogen[A22]): Cogen[Z] =
    tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22).contramap(t => f(t).get)

}
