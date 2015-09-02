package scalaprops

import scodec.bits.{BitVector, ByteVector}
import scodec.interop.scalaz._
import scodec._

import scalaprops.FunctionEqual._
import scalaz.{Apply, Equal}
import scalaz.std.anyVal._
import scalaz.std.tuple._

object ScodecTest extends Scalaprops {

  private[this] implicit val strGen = Gen.alphaNumString

  private[this] implicit val bitVectorGen: Gen[BitVector] =
    Gen[List[Byte]].map(BitVector(_))

  private[this] implicit val bitVectorCogen: Cogen[BitVector] =
    Cogen[Array[Byte]].contramap(_.toByteArray)

  private[this] implicit val byteVectorGen: Gen[ByteVector] =
    Gen[List[Byte]].map(ByteVector(_))

  private[this] implicit val errGen: Gen[Err] =
    Gen.oneOf(
      Gen.from2(Err.General),
      Gen.from3(Err.InsufficientBits),
      Apply[Gen].apply2(Gen[Byte], Gen[List[String]])(Err.MatchingDiscriminatorNotFound(_, _))
    )

  private[this] implicit def attemptGen[A](implicit A: Gen[A]): Gen[Attempt[A]] =
    Gen.oneOf(
      Gen.from(Attempt.failure[A] _),
      A.map(Attempt.successful)
    )

  private[this] implicit def decodeResultGen[A: Gen]: Gen[DecodeResult[A]] =
    Gen.from2(DecodeResult[A])

  private[this] implicit def decodeResultCogen[A: Cogen]: Cogen[DecodeResult[A]] =
    Cogen.from2(DecodeResult.unapply[A])

  private[this] implicit def decoderGen[A: Gen]: Gen[Decoder[A]] =
    Gen[BitVector => Attempt[DecodeResult[A]]].map(Decoder[A])

  private[this] implicit def decoderEqual[A: Equal]: Equal[Decoder[A]] =
    Equal[BitVector => Attempt[DecodeResult[A]]].contramap(d => d.decode(_))

  private[this] implicit def encoderGen[A: Cogen]: Gen[Encoder[A]] =
    Gen[A => Attempt[BitVector]].map(Encoder[A])

  private[this] implicit def encoderEqual[A: Gen]: Equal[Encoder[A]] =
    Equal[A => Attempt[BitVector]].contramap(e => e.encode(_))

  private[this] implicit def genCodecGen[A: Cogen, B: Gen]: Gen[GenCodec[A, B]] =
    Gen.from2(GenCodec.apply[A, B])

  private[this] implicit def genCodecEqual[A: Gen, B: Equal]: Equal[GenCodec[A, B]] =
    Equal.equalBy((g: GenCodec[A, B]) => (g.asDecoder, g.asEncoder))

  private[this] implicit def codecGen[A: Cogen: Gen]: Gen[Codec[A]] =
    Apply[Gen].apply2(Gen[Encoder[A]], Gen[Decoder[A]])(Codec(_, _))

  private[this] implicit def codecEqual[A: Gen: Equal]: Equal[Codec[A]] =
    Equal.equalBy((g: Codec[A]) => (g.asDecoder, g.asEncoder))

  val testBitVector = Properties.list(
    scalazlaws.monoid.all[BitVector],
    scalazlaws.equal.all[BitVector]
  )

  val testByteVector = Properties.list(
    scalazlaws.monoid.all[ByteVector],
    scalazlaws.equal.all[ByteVector]
  )

  val testAttempt = Properties.list(
    scalazlaws.monad.all[Attempt],
    scalazlaws.equal.all[Attempt[Byte]]
  )

  val testDecodeResult = Properties.list(
    scalazlaws.comonad.all[DecodeResult],
    scalazlaws.equal.all[DecodeResult[Byte]],
    scalazlaws.traverse.all[DecodeResult]
  )

  val testDecoder = Properties.list(
    scalazlaws.monad.all[Decoder],
    scalazlaws.equal.all[Decoder[Byte]]
  )

  val testEncoder = Properties.list(
    scalazlaws.contravariant.all[Encoder],
    scalazlaws.equal.all[Encoder[Byte]]
  )

  val testGenCodec = Properties.list(
    scalazlaws.profunctor.all[GenCodec],
    scalazlaws.equal.all[GenCodec[Byte, Byte]]
  )

  val testCodec = Properties.list(
    scalazlaws.invariantFunctor.all[Codec],
    scalazlaws.equal.all[Codec[Byte]]
  )

}
