package scalaprops

object ArgumentsTest extends Scalaprops {

  val parse = Property.forAll {
    val args = List(
      "--only",
      "foo",
      "bar",
      "--showDuration=invalid",
      "--showDuration=10",
      "--showDuration=foo=bar",
      "--showDuration=",
      "--showDuration=30",
      "--showDuration=",
      "--showDuration=invalid",
      "--seed=100",
      "--maxSize=9999999",
      "--timeout=25",
      "--minSuccessful=256",
      "--maxDiscarded=1024"
    )
    val result = Arguments.parse(args)
    val expect = Arguments(
      only = List("foo", "bar"),
      showDuration = 30,
      param = ParamOpt(
        seed = Some(Seed.IntSeed(100)),
        minSuccessful = Some(256),
        maxDiscarded = Some(1024),
        minSize = None,
        maxSize = Some(9999999),
        timeoutSeconds = Some(25)
      )
    )
    assert(result == expect, result)
    true
  }

}
