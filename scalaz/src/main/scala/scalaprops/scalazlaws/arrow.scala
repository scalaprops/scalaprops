package scalaprops
package scalazlaws

import scalaprops.Property.forAll
import scalaz._

object arrow {
  def identity[=>:[_, _]: Arrow, A](implicit E: Equal[A =>: A]): Property =
    forAll(ArrowLaws[=>:].arrowIdentity[A])

  def composition[=>:[_, _]: Arrow, A, B, C](implicit ab: Gen[A => B], bc: Gen[B => C], E: Equal[A =>: C]): Property =
    forAll(ArrowLaws[=>:].arrowComposition[A, B, C] _)

  def extension[=>:[_, _]: Arrow, A, B, C](implicit ab: Gen[A => B], E: Equal[(A, C) =>: (B, C)]): Property =
    forAll(ArrowLaws[=>:].arrowExtension[A, B, C] _)

  def functor[=>:[_, _]: Arrow, A, B, C, D](
    implicit ab: Gen[A =>: B],
    bc: Gen[B =>: C],
    E: Equal[(A, D) =>: (C, D)]
  ): Property =
    forAll(ArrowLaws[=>:].arrowFunctor[A, B, C, D] _)

  def exchange[=>:[_, _]: Arrow, A, B, C, D](
    implicit f: Gen[A =>: B],
    g: Gen[C => D],
    E: Equal[(A, C) =>: (B, D)]
  ): Property =
    forAll(ArrowLaws[=>:].arrowExchange[A, B, C, D] _)

  def unit[=>:[_, _]: Arrow, A, B, C](implicit f: Gen[A =>: B], E: Equal[(A, C) =>: B]): Property =
    forAll(ArrowLaws[=>:].arrowUnit[A, B, C] _)

  def association[=>:[_, _]: Arrow, A, B, C, D](
    implicit f: Gen[A =>: B],
    E: Equal[((A, C), D) =>: (B, (C, D))]
  ): Property =
    forAll(ArrowLaws[=>:].arrowAssociation[A, B, C, D] _)

  def laws[=>:[_, _]: Arrow](
    implicit
    A1: Equal[Int =>: Int],
    A2: Gen[Int =>: Int],
    A3: Equal[(Int, Int) =>: (Int, Int)],
    A4: Equal[(Int, Int) =>: Int],
    A5: Equal[((Int, Int), Int) =>: (Int, (Int, Int))]
  ) = Properties.properties(ScalazLaw.arrow)(
    ScalazLaw.arrowIdentity -> identity[=>:, Int],
    ScalazLaw.arrowComposition -> composition[=>:, Int, Int, Int],
    ScalazLaw.arrowExtension -> extension[=>:, Int, Int, Int],
    ScalazLaw.arrowFunctor -> functor[=>:, Int, Int, Int, Int],
    ScalazLaw.arrowExchange -> exchange[=>:, Int, Int, Int, Int],
    ScalazLaw.arrowUnit -> unit[=>:, Int, Int, Int],
    ScalazLaw.arrowAssociation -> association[=>:, Int, Int, Int, Int]
  )

  def all[=>:[_, _]: Arrow](
    implicit
    A1: Equal[Int =>: Int],
    A2: Gen[Int =>: Int],
    A3: Equal[(Int, Int) =>: (Int, Int)],
    A4: Equal[(Int, Int) =>: Int],
    A5: Equal[((Int, Int), Int) =>: (Int, (Int, Int))]
  ) = Properties.fromProps(ScalazLaw.arrowAll, arrow.laws[=>:], category.all[=>:], profunctor.all[=>:])
}
