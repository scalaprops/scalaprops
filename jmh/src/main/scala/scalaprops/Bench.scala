package scalaprops

import org.openjdk.jmh.annotations.Benchmark
import scala.util.Random

class Bench {

  @Benchmark
  def randInt(): Unit = {
    var r = Rand.standard(System.nanoTime)
    var i = 0
    val X = Random.nextInt
    val Y = Random.nextInt
    while(i < 100000) {
      r = r.choose(X, Y)._1
      i += 1
    }
  }

  @Benchmark
  def randLong(): Unit = {
    var r = Rand.standard(System.nanoTime)
    var i = 0
    val X = Random.nextLong
    val Y = X + Random.nextInt
    while(i < 100000) {
      r = r.chooseLong(X, Y)._1
      i += 1
    }
  }

}
