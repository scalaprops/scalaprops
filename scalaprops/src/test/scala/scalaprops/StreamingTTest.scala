package cats.data

import scalaprops._
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.tuple._
import scalaz.std.list._

object StreamingTTest extends Scalaprops {

  type StreamTList[A] = cats.data.StreamingT[List, A]

  override def param = super.param.copy(maxSize = 2, minSuccessful = 1000)

/**
  implicit def streamingListShrink[A: Shrink]: Shrink[StreamTList[A]] =
    Shrink[List[List[A]]].xmap(
      x => StreamingT.fromList(x.flatten),
      _.toList
    )
    */


  implicit def streamingListShrink[A]: Shrink[StreamTList[A]] =
    Shrink.empty

  val showingAndShrinkingFunction = {
    type A = Boolean
    scalazlaws.bindS.associativity[StreamTList, A, A, A]
  }

}
