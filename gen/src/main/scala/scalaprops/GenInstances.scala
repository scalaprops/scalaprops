package scalaprops

import scalaz._
import Gen.f1

abstract class GenInstances private[scalaprops] {

  implicit final def f2[A1, A2, Z](implicit A1: Cogen[A1], A2: Cogen[A2], Z: Gen[Z]): Gen[(A1, A2) => Z] =
    f1(A1, f1(A2, Z)).map(f => (a1, a2) => f(a1)(a2))

  implicit final def tuple2[A1, A2](implicit A1: Gen[A1], A2: Gen[A2]): Gen[Tuple2[A1, A2]] =
    from2[A1, A2, Tuple2[A1, A2]](Tuple2.apply)(A1, A2)

  final def from[A1, A2, Z](f: (A1, A2) => Z)(implicit A1: Gen[A1], A2: Gen[A2]): Gen[Z] =
    from2(f)(A1, A2)

  final def from2[A1, A2, Z](f: (A1, A2) => Z)(implicit A1: Gen[A1], A2: Gen[A2]): Gen[Z] =
    Apply[Gen].apply2(A1, A2)(f)

  implicit final def f3[A1, A2, A3, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], Z: Gen[Z]): Gen[(A1, A2, A3) => Z] =
    f1(A1, f1(A2, f1(A3, Z))).map(f => (a1, a2, a3) => f(a1)(a2)(a3))

  implicit final def tuple3[A1, A2, A3](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Gen[Tuple3[A1, A2, A3]] =
    from3[A1, A2, A3, Tuple3[A1, A2, A3]](Tuple3.apply)(A1, A2, A3)

  final def from[A1, A2, A3, Z](f: (A1, A2, A3) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Gen[Z] =
    from3(f)(A1, A2, A3)

  final def from3[A1, A2, A3, Z](f: (A1, A2, A3) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Gen[Z] =
    Apply[Gen].apply3(A1, A2, A3)(f)

  implicit final def f4[A1, A2, A3, A4, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], Z: Gen[Z]): Gen[(A1, A2, A3, A4) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, Z)))).map(f => (a1, a2, a3, a4) => f(a1)(a2)(a3)(a4))

  implicit final def tuple4[A1, A2, A3, A4](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4]): Gen[Tuple4[A1, A2, A3, A4]] =
    from4[A1, A2, A3, A4, Tuple4[A1, A2, A3, A4]](Tuple4.apply)(A1, A2, A3, A4)

  final def from[A1, A2, A3, A4, Z](f: (A1, A2, A3, A4) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4]): Gen[Z] =
    from4(f)(A1, A2, A3, A4)

  final def from4[A1, A2, A3, A4, Z](f: (A1, A2, A3, A4) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4]): Gen[Z] =
    Apply[Gen].apply4(A1, A2, A3, A4)(f)

  implicit final def f5[A1, A2, A3, A4, A5, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, Z))))).map(f => (a1, a2, a3, a4, a5) => f(a1)(a2)(a3)(a4)(a5))

  implicit final def tuple5[A1, A2, A3, A4, A5](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5]): Gen[Tuple5[A1, A2, A3, A4, A5]] =
    from5[A1, A2, A3, A4, A5, Tuple5[A1, A2, A3, A4, A5]](Tuple5.apply)(A1, A2, A3, A4, A5)

  final def from[A1, A2, A3, A4, A5, Z](f: (A1, A2, A3, A4, A5) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5]): Gen[Z] =
    from5(f)(A1, A2, A3, A4, A5)

  final def from5[A1, A2, A3, A4, A5, Z](f: (A1, A2, A3, A4, A5) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5]): Gen[Z] =
    Apply[Gen].apply5(A1, A2, A3, A4, A5)(f)

  implicit final def f6[A1, A2, A3, A4, A5, A6, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, Z)))))).map(f => (a1, a2, a3, a4, a5, a6) => f(a1)(a2)(a3)(a4)(a5)(a6))

  implicit final def tuple6[A1, A2, A3, A4, A5, A6](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6]): Gen[Tuple6[A1, A2, A3, A4, A5, A6]] =
    from6[A1, A2, A3, A4, A5, A6, Tuple6[A1, A2, A3, A4, A5, A6]](Tuple6.apply)(A1, A2, A3, A4, A5, A6)

  final def from[A1, A2, A3, A4, A5, A6, Z](f: (A1, A2, A3, A4, A5, A6) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6]): Gen[Z] =
    from6(f)(A1, A2, A3, A4, A5, A6)

  final def from6[A1, A2, A3, A4, A5, A6, Z](f: (A1, A2, A3, A4, A5, A6) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6]): Gen[Z] =
    Apply[Gen].apply6(A1, A2, A3, A4, A5, A6)(f)

  implicit final def f7[A1, A2, A3, A4, A5, A6, A7, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, Z))))))).map(f => (a1, a2, a3, a4, a5, a6, a7) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7))

  implicit final def tuple7[A1, A2, A3, A4, A5, A6, A7](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7]): Gen[Tuple7[A1, A2, A3, A4, A5, A6, A7]] =
    from7[A1, A2, A3, A4, A5, A6, A7, Tuple7[A1, A2, A3, A4, A5, A6, A7]](Tuple7.apply)(A1, A2, A3, A4, A5, A6, A7)

  final def from[A1, A2, A3, A4, A5, A6, A7, Z](f: (A1, A2, A3, A4, A5, A6, A7) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7]): Gen[Z] =
    from7(f)(A1, A2, A3, A4, A5, A6, A7)

  final def from7[A1, A2, A3, A4, A5, A6, A7, Z](f: (A1, A2, A3, A4, A5, A6, A7) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7]): Gen[Z] =
    Apply[Gen].apply7(A1, A2, A3, A4, A5, A6, A7)(f)

  implicit final def f8[A1, A2, A3, A4, A5, A6, A7, A8, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, Z)))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8))

  implicit final def tuple8[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8]): Gen[Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]] =
    from8[A1, A2, A3, A4, A5, A6, A7, A8, Tuple8[A1, A2, A3, A4, A5, A6, A7, A8]](Tuple8.apply)(A1, A2, A3, A4, A5, A6, A7, A8)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8]): Gen[Z] =
    from8(f)(A1, A2, A3, A4, A5, A6, A7, A8)

  final def from8[A1, A2, A3, A4, A5, A6, A7, A8, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8]): Gen[Z] =
    Apply[Gen].apply8(A1, A2, A3, A4, A5, A6, A7, A8)(f)

  implicit final def f9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, Z))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9))

  implicit final def tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9]): Gen[Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]] =
    from9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9]](Tuple9.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9]): Gen[Z] =
    from9(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9)

  final def from9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9]): Gen[Z] =
    Apply[Gen].apply9(A1, A2, A3, A4, A5, A6, A7, A8, A9)(f)

  implicit final def f10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, Z)))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10))

  implicit final def tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10]): Gen[Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]] =
    from10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]](Tuple10.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10]): Gen[Z] =
    from10(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)

  final def from10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10]): Gen[Z] =
    Apply[Gen].apply10(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)(f)

  implicit final def f11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, Z))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11))

  implicit final def tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11]): Gen[Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]] =
    from11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]](Tuple11.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11]): Gen[Z] =
    from11(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)

  final def from11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11]): Gen[Z] =
    Apply[Gen].apply11(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)(f)

  implicit final def f12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, Z)))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12))

  implicit final def tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12]): Gen[Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]] =
    from12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]](Tuple12.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12]): Gen[Z] =
    from12(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)

  final def from12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12]): Gen[Z] =
    Apply[Gen].apply12(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)(f)

  implicit final def f13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, Z))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13))

  implicit final def tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13]): Gen[Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]] =
    from13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]](Tuple13.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13]): Gen[Z] =
    from13(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)

  final def from13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13]): Gen[Z] =
    Apply[Gen].apply2(
      tuple6(A1, A2, A3, A4, A5, A6),
      tuple7(A7, A8, A9, A10, A11, A12, A13)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7)
    )

  implicit final def f14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, Z)))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14))

  implicit final def tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14]): Gen[Tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]] =
    from14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]](Tuple14.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14]): Gen[Z] =
    from14(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)

  final def from14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14]): Gen[Z] =
    Apply[Gen].apply2(
      tuple7(A1, A2, A3, A4, A5, A6, A7),
      tuple7(A8, A9, A10, A11, A12, A13, A14)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7)
    )

  implicit final def f15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, Z))))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15))

  implicit final def tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15]): Gen[Tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]] =
    from15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]](Tuple15.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15]): Gen[Z] =
    from15(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)

  final def from15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15]): Gen[Z] =
    Apply[Gen].apply2(
      tuple7(A1, A2, A3, A4, A5, A6, A7),
      tuple8(A8, A9, A10, A11, A12, A13, A14, A15)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t2._8)
    )

  implicit final def f16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, Z)))))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16))

  implicit final def tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16]): Gen[Tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]] =
    from16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]](Tuple16.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16]): Gen[Z] =
    from16(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)

  final def from16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16]): Gen[Z] =
    Apply[Gen].apply2(
      tuple8(A1, A2, A3, A4, A5, A6, A7, A8),
      tuple8(A9, A10, A11, A12, A13, A14, A15, A16)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t1._8, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t2._8)
    )

  implicit final def f17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, Z))))))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17))

  implicit final def tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17]): Gen[Tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]] =
    from17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]](Tuple17.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17]): Gen[Z] =
    from17(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)

  final def from17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17]): Gen[Z] =
    Apply[Gen].apply2(
      tuple8(A1, A2, A3, A4, A5, A6, A7, A8),
      tuple9(A9, A10, A11, A12, A13, A14, A15, A16, A17)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t1._8, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t2._8, t2._9)
    )

  implicit final def f18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, Z)))))))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18))

  implicit final def tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18]): Gen[Tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]] =
    from18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]](Tuple18.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18]): Gen[Z] =
    from18(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)

  final def from18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18]): Gen[Z] =
    Apply[Gen].apply2(
      tuple9(A1, A2, A3, A4, A5, A6, A7, A8, A9),
      tuple9(A10, A11, A12, A13, A14, A15, A16, A17, A18)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t1._8, t1._9, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t2._8, t2._9)
    )

  implicit final def f19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, f1(A19, Z))))))))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19))

  implicit final def tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19]): Gen[Tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]] =
    from19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]](Tuple19.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19]): Gen[Z] =
    from19(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)

  final def from19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19]): Gen[Z] =
    Apply[Gen].apply2(
      tuple9(A1, A2, A3, A4, A5, A6, A7, A8, A9),
      tuple10(A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t1._8, t1._9, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t2._8, t2._9, t2._10)
    )

  implicit final def f20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, f1(A19, f1(A20, Z)))))))))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19)(a20))

  implicit final def tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20]): Gen[Tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]] =
    from20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]](Tuple20.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20]): Gen[Z] =
    from20(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)

  final def from20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20]): Gen[Z] =
    Apply[Gen].apply2(
      tuple10(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10),
      tuple10(A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t1._8, t1._9, t1._10, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t2._8, t2._9, t2._10)
    )

  implicit final def f21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], A21: Cogen[A21], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, f1(A19, f1(A20, f1(A21, Z))))))))))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19)(a20)(a21))

  implicit final def tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], A21: Gen[A21]): Gen[Tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]] =
    from21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]](Tuple21.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], A21: Gen[A21]): Gen[Z] =
    from21(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)

  final def from21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], A21: Gen[A21]): Gen[Z] =
    Apply[Gen].apply2(
      tuple10(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10),
      tuple11(A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t1._8, t1._9, t1._10, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t2._8, t2._9, t2._10, t2._11)
    )

  implicit final def f22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](implicit A1: Cogen[A1], A2: Cogen[A2], A3: Cogen[A3], A4: Cogen[A4], A5: Cogen[A5], A6: Cogen[A6], A7: Cogen[A7], A8: Cogen[A8], A9: Cogen[A9], A10: Cogen[A10], A11: Cogen[A11], A12: Cogen[A12], A13: Cogen[A13], A14: Cogen[A14], A15: Cogen[A15], A16: Cogen[A16], A17: Cogen[A17], A18: Cogen[A18], A19: Cogen[A19], A20: Cogen[A20], A21: Cogen[A21], A22: Cogen[A22], Z: Gen[Z]): Gen[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z] =
    f1(A1, f1(A2, f1(A3, f1(A4, f1(A5, f1(A6, f1(A7, f1(A8, f1(A9, f1(A10, f1(A11, f1(A12, f1(A13, f1(A14, f1(A15, f1(A16, f1(A17, f1(A18, f1(A19, f1(A20, f1(A21, f1(A22, Z)))))))))))))))))))))).map(f => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) => f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19)(a20)(a21)(a22))

  implicit final def tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], A21: Gen[A21], A22: Gen[A22]): Gen[Tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]] =
    from22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]](Tuple22.apply)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)

  final def from[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], A21: Gen[A21], A22: Gen[A22]): Gen[Z] =
    from22(f)(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)

  final def from22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], A6: Gen[A6], A7: Gen[A7], A8: Gen[A8], A9: Gen[A9], A10: Gen[A10], A11: Gen[A11], A12: Gen[A12], A13: Gen[A13], A14: Gen[A14], A15: Gen[A15], A16: Gen[A16], A17: Gen[A17], A18: Gen[A18], A19: Gen[A19], A20: Gen[A20], A21: Gen[A21], A22: Gen[A22]): Gen[Z] =
    Apply[Gen].apply2(
      tuple11(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11),
      tuple11(A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)
    )((t1, t2) =>
      f(t1._1, t1._2, t1._3, t1._4, t1._5, t1._6, t1._7, t1._8, t1._9, t1._10, t1._11, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t2._8, t2._9, t2._10, t2._11)
    )

}
