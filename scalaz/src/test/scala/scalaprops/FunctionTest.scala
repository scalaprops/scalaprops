package scalaprops

import scalaz.std.function._
import scalaz.std.anyVal._
import FunctionEqual._

object FunctionTest extends Scalaprops {

  type A1 = Byte
  type A2 = Byte
  type A3 = Byte
  type A4 = Byte
  type A5 = Byte
  type A6 = Byte
  type A7 = Byte
  type A8 = Byte

  val function2 = scalazlaws.monad.all[({type l[a] = (A1, A2) => a})#l]
  val function3 = scalazlaws.monad.all[({type l[a] = (A1, A2, A3) => a})#l]
  val function4 = scalazlaws.monad.all[({type l[a] = (A1, A2, A3, A4) => a})#l]
  val function5 = scalazlaws.monad.all[({type l[a] = (A1, A2, A3, A4, A5) => a})#l]
  val function6 = scalazlaws.monad.all[({type l[a] = (A1, A2, A3, A4, A5, A6) => a})#l]
  val function7 = scalazlaws.monad.all[({type l[a] = (A1, A2, A3, A4, A5, A6, A7) => a})#l]
  val function8 = scalazlaws.monad.all[({type l[a] = (A1, A2, A3, A4, A5, A6, A7, A8) => a})#l]

  val function0BindRec = scalazlaws.bindRec.laws[Function0]

  val function2BindRec = scalazlaws.bindRec.laws[({type l[a] = (A1, A2) => a})#l]
  val function3BindRec = scalazlaws.bindRec.laws[({type l[a] = (A1, A2, A3) => a})#l]
  val function4BindRec = scalazlaws.bindRec.laws[({type l[a] = (A1, A2, A3, A4) => a})#l]
  val function5BindRec = scalazlaws.bindRec.laws[({type l[a] = (A1, A2, A3, A4, A5) => a})#l]
  val function6BindRec = scalazlaws.bindRec.laws[({type l[a] = (A1, A2, A3, A4, A5, A6) => a})#l]
  val function7BindRec = scalazlaws.bindRec.laws[({type l[a] = (A1, A2, A3, A4, A5, A6, A7) => a})#l]
  val function8BindRec = scalazlaws.bindRec.laws[({type l[a] = (A1, A2, A3, A4, A5, A6, A7, A8) => a})#l]
}
