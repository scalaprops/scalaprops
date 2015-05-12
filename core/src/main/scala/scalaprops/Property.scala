package scalaprops

import java.util.concurrent.atomic.AtomicBoolean
import scalaz._

final case class Property(f: (Int, Rand) => (Result, Rand)) {
  def toCheck: Check =
    Check(this)

  def toCheckWith(endo: Endo[Param]): Check =
    Check(this, endo)

  def resize(size: Int): Property =
    Property.fromGen(gen.resize(size))

  def mapSize(g: Int => Int): Property =
    Property.fromGen(gen.mapSize(g))

  def gen: Gen[Result] = Gen.gen(f)

  def and(p: Property): Property =
    Property.fromGen(
      Apply[Gen].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isFalsified){
          res1
        }else if(res2.isException || res2.isFalsified){
          res2
        }else if(res1.isProven || res1.isUnfalsified){
          res2
        }else if(res2.isProven || res2.isUnfalsified){
          res1
        }else Result.NoResult
      )
    )

  def or(p: Property): Property =
    Property.fromGen(
      Apply[Gen].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isFalsified){
          res1
        }else if(res2.isException || res2.isFalsified){
          res2
        }else if(res1.isProven || res1.isUnfalsified){
          res1
        }else if(res2.isProven || res2.isUnfalsified){
          res2
        }else Result.NoResult
      )
    )

  def sequence(p: Property): Property =
    Property.fromGen(
      Apply[Gen].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isProven || res1.isUnfalsified) {
          res1
        }else if(res2.isException || res2.isProven || res2.isUnfalsified){
          res2
        }else if(res1.isFalsified){
          res2
        }else if(res2.isFalsified){
          res1
        }else Result.NoResult
      )
    )

  // TODO remove `listener` parameter? use scalaz-stream?
  def check(param: Param, cancel: AtomicBoolean, listener: Int => Unit): CheckResult = {
    import param.{rand => _, _}
    @annotation.tailrec
    def loop(s: Int, discarded: Int, sz: Float, random: Rand): CheckResult = if(cancel.get()) {
      CheckResult.Timeout(s, discarded)
    }else{
      val size = {
        if (s == 0 && discarded == 0) minSize
        else sz + (maxSize - sz) / (minSuccessful - s)
      }

      val r = \/.fromTryCatchThrowable[(Result, Rand), Throwable](
        f(math.round(size), random)
      )

      r match {
        case \/-((Result.NoResult, nextRand)) =>
          if (discarded + 1 >= maxDiscarded) {
            CheckResult.Exhausted(s, discarded + 1)
          } else {
            loop(s, discarded + 1, size, nextRand)
          }
        case \/-((Result.Proven, _)) =>
          CheckResult.Proven(s + 1, discarded)
        case \/-((Result.Unfalsified(args), nextRand)) =>
          if (s + 1 >= minSuccessful) {
            CheckResult.Passed(s + 1, discarded)
          } else {
            listener(s)
            loop(s + 1, discarded, size, nextRand)
          }
        case \/-((Result.Falsified(args), _)) =>
          CheckResult.Falsified(s, discarded, args)
        case \/-((Result.Exception(args, ex), _)) =>
          CheckResult.PropException(s, discarded, args, ex)
        case \/-((Result.Ignored(reason), _)) =>
          CheckResult.Ignored(s, discarded, reason)
        case -\/(e) =>
          CheckResult.GenException(s, discarded, e)
      }
    }

    loop(0, 0, minSize, param.rand)
  }

  def toProperties[A](id: A): Properties[A] =
    Properties.single(id, this)

  def ignore(reason: String): Property =
    Property((_, rand) => (Result.Ignored(reason), rand))
}

object Property {
  private[this] val noResult = Property((_, r) => (Result.NoResult, r))

  def implies(b: => Boolean, p: => Property): Property =
    if(b) {
      p
    } else {
      noResult
    }


  def fromGen(g: Gen[Result]): Property =
    Property(g.f)

  def propFromResult(r: Result): Property =
    Property((_, rand) => (r, rand))

  val prop: Boolean => Property = b => propFromResult{
    if(b) Result.Proven
    else Result.Falsified(IList.empty)
  }

  def forall0[A](g: Gen[A], shrink: Shrink[A])(f: A => Property): Property =
    Property((i, r) => {
      val failed: Maybe[(A, Result, Rand)] => Boolean = _.exists(_._2.failed)
      def first(as: Stream[(A, Rand)], shrinks: Int): Maybe[(A, Result, Rand)] = {
        as.map{ case (a, rr) =>
          val x = exception(f(a)).f(i, rr)
          x._1.toMaybe.map(result =>
            (a, result.provenAsUnfalsified.addArg(Arg(a, shrinks)): Result, x._2)
          )
        } match {
          case Stream() =>
            Maybe.empty
          case results @ (h #:: _)=>
            results.find(failed).getOrElse(h)
        }
      }

      first(Stream(g.f(i, r)), 0) match {
        case xx @ Maybe.Just((a, re, rand)) if re.failed =>
          @annotation.tailrec
          def loop(shrinks: Int, x: Maybe[(A, Result, Rand)]): Maybe[(Result, Rand)] = {
            val x0 = first(shrink(a).map(_ -> rand.next), shrinks)
            if(failed(x0)) {
              loop(shrinks + 1, x0)
            } else {
              x.map(t => (t._2, t._3))
            }
          }
          loop(1, xx).getOrElse((Result.NoResult, Rand.standard(0)))
        case xx =>
          xx.map(t => (t._2, t._3)).getOrElse((Result.NoResult, Rand.standard(0)))
      }
    })

  def exception(p: => Property): Property =
    try {
      p
    } catch {
      case t: Throwable =>
        Property((i, r) => Result.Exception(IList.empty, t) -> r)
    }

  def forAll[A1](f: A1 => Boolean)(implicit A1: Gen[A1]): Property =
    forall0(A1, Shrink.empty)(f.andThen(prop))

  def forAll[A1, A2](f: (A1, A2) => Boolean)(implicit A1: Gen[A1], A2: Gen[A2]): Property =
    forall0(A1, Shrink.empty)(a1 =>
      forall0(A2, Shrink.empty)(a2 =>
        prop(f(a1, a2))
      )
    )

  def forAll[A1, A2, A3](f: (A1, A2, A3) => Boolean)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3]): Property =
    forall0(A1, Shrink.empty)(a1 =>
      forall0(A2, Shrink.empty)(a2 =>
        forall0(A3, Shrink.empty)(a3 =>
          prop(f(a1, a2, a3))
        )
      )
    )

  def forAll[A1, A2, A3, A4](f: (A1, A2, A3, A4) => Boolean)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4]): Property =
    forall0(A1, Shrink.empty)(a1 =>
      forall0(A2, Shrink.empty)(a2 =>
        forall0(A3, Shrink.empty)(a3 =>
          forall0(A4, Shrink.empty)(a4 =>
            prop(f(a1, a2, a3, a4))
          )
        )
      )
    )

  def forAll[A1, A2, A3, A4, A5](f: (A1, A2, A3, A4, A5) => Boolean)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5]): Property =
    forall0(A1, Shrink.empty)(a1 =>
      forall0(A2, Shrink.empty)(a2 =>
        forall0(A3, Shrink.empty)(a3 =>
          forall0(A4, Shrink.empty)(a4 =>
            forall0(A5, Shrink.empty)(a5 =>
              prop(f(a1, a2, a3, a4, a5))
            )
          )
        )
      )
    )

  def property1[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1]): Property =
    forall0(A1, S1)(f)

  def property2[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        f(a1, a2)
      )
    )

  def property3[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        forall0(A3, S3)(a3 =>
          f(a1, a2, a3)
        )
      )
    )

  def property[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1]): Property =
    property1(f)

  def property[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): Property =
    property2(f)

  def property[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): Property =
    property3(f)

  object NoShrink {
    def property1[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1] = Shrink.empty[A1]): Property =
      Property.property1(f)

    def property2[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1] = Shrink.empty[A1], S2: Shrink[A2] = Shrink.empty[A2]): Property =
      Property.property2(f)

    def property3[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1] = Shrink.empty[A1], S2: Shrink[A2] = Shrink.empty[A2], S3: Shrink[A3] = Shrink.empty[A3]): Property =
      Property.property3(f)
  }

}
