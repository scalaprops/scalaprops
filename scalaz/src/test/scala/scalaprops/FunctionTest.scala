package scalaprops

import FunctionEqual.*
import scalaz.std.anyVal.*
import scalaz.std.function.*

object FunctionTest extends Scalaprops {
  type A1 = Byte
  type A2 = Byte
  type A3 = Byte
  type A4 = Byte
  type A5 = Byte
  type A6 = Byte
  type A7 = Byte
  type A8 = Byte

  property("function2") = scalazlaws.monad.all[({ type l[a] = (A1, A2) => a })#l]
  property("function3") = scalazlaws.monad.all[({ type l[a] = (A1, A2, A3) => a })#l]
  property("function4") = scalazlaws.monad.all[({ type l[a] = (A1, A2, A3, A4) => a })#l]
  property("function5") = scalazlaws.monad.all[({ type l[a] = (A1, A2, A3, A4, A5) => a })#l]
  property("function6") = scalazlaws.monad.all[({ type l[a] = (A1, A2, A3, A4, A5, A6) => a })#l]
  property("function7") = scalazlaws.monad.all[({ type l[a] = (A1, A2, A3, A4, A5, A6, A7) => a })#l]
  property("function8") = scalazlaws.monad.all[({ type l[a] = (A1, A2, A3, A4, A5, A6, A7, A8) => a })#l]

  property("function0BindRec") = scalazlaws.bindRec.laws[Function0]

  property("function2BindRec") = scalazlaws.bindRec.laws[({ type l[a] = (A1, A2) => a })#l]
  property("function3BindRec") = scalazlaws.bindRec.laws[({ type l[a] = (A1, A2, A3) => a })#l]
  property("function4BindRec") = scalazlaws.bindRec.laws[({ type l[a] = (A1, A2, A3, A4) => a })#l]
  property("function5BindRec") = scalazlaws.bindRec.laws[({ type l[a] = (A1, A2, A3, A4, A5) => a })#l]
  property("function6BindRec") = scalazlaws.bindRec.laws[({ type l[a] = (A1, A2, A3, A4, A5, A6) => a })#l]
  property("function7BindRec") = scalazlaws.bindRec.laws[({ type l[a] = (A1, A2, A3, A4, A5, A6, A7) => a })#l]
  property("function8BindRec") = scalazlaws.bindRec.laws[({ type l[a] = (A1, A2, A3, A4, A5, A6, A7, A8) => a })#l]
}
