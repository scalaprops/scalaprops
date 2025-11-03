package scalaprops

import java.util.Arrays

@scalajs.js.annotation.JSExportAll
object TinyMT32Test extends Scalaprops {
  val testGetDefault = Property.forAll {
    var tiny = TinyMT32.getDefault(1)
    val c_data = List(0x97b6d625, 0x3a86e2e1, 0xdd7305b1, 0x8e4ef1b0, 0xd60a5515, 0xe3b751f6, 0x7e073136, 0x82e5df8b,
      0xa5e6b3a8, 0x2d91deed)
    val output = (0 until c_data.length).map { _ =>
      val temp = tiny.nextInt
      tiny = temp._1
      temp._2
    }
    assert(output == c_data)
    val weight: Int = 63
    val delta: Int = 0
    assert("d8524022ed8dff4a8dcc50c798faba43" == tiny.getCharacteristic)
    assert(0 == tiny.getId)
    assert(delta == tiny.getDelta)
    weight == tiny.getWeight
  }

  val testGetDefaultString = Property.forAll {
    var tiny = TinyMT32.getDefault("abcd")
    val c_data = List(0x92264019, 0x3e048b87, 0x0618bd2a, 0x0b85252c, 0xf720f342, 0x73ac4297, 0x20974815, 0x8ac7a201,
      0x7443b188, 0x0b7c6501)

    val output = (0 until c_data.length).map { _ =>
      val temp = tiny.nextInt
      tiny = temp._1
      temp._2
    }
    output == c_data
  }

  val testGetDefaultSeedArray = Property.forAll {
    val seeds = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    var tiny = TinyMT32.getDefault(seeds)
    val c_data = List(0x7c14361a, 0x41b450a8, 0xfecc94ae, 0xd19693cd, 0x1d8b3ef7, 0xb5c88b3f, 0x45c79c71, 0xc8465811,
      0x10fcf027, 0x4e3317e5)

    val output = (0 until c_data.length).map { _ =>
      val temp = tiny.nextInt
      tiny = temp._1
      temp._2
    }
    output == c_data
  }

  val testGetDefaultArray = Property.forAll {
    val tiny = TinyMT32.getDefaultArray(5, 1L, 1L)
    val c_data = Vector[Vector[Int]](
      Vector(0x97b6d625, 0x3a86e2e1, 0xdd7305b1, 0x8e4ef1b0, 0xd60a5515, 0xe3b751f6, 0x7e073136, 0x82e5df8b, 0xa5e6b3a8,
        0x2d91deed),
      Vector(0x069dd221, 0x139591eb, 0xb1bb01cb, 0xa59dfffc, 0x739f69a3, 0xa73eb87e, 0xe74ff5a4, 0x66add33a, 0x8058ebab,
        0xd40b22ae),
      Vector(0x04788045, 0x4ff9f6ac, 0xa2320522, 0x7b54307f, 0x54d3cee9, 0xa6dd6ea1, 0xceaa10dc, 0x0ed65e8d, 0xe1457e91,
        0x154a1ab8),
      Vector(0xd026d1ca, 0xebce67c6, 0xdc706eda, 0xbc8007cb, 0xe2eb9899, 0x399992f3, 0x0ab705e6, 0x8b79b94b, 0x8a5c9316,
        0x16cc039f),
      Vector(0xd15c9d16, 0xa767e149, 0xe9c73b4b, 0xaf69de00, 0x8319be3b, 0xecc79680, 0x8ab49653, 0x9bf1d861, 0x04c7dab6,
        0xb0d5cf24)
    )
    c_data.zipWithIndex.foreach { case (data, i) =>
      val output = (0 until data.size).map { _ =>
        val temp = tiny(i).nextInt
        tiny(i) = temp._1
        temp._2
      }
      assert(output == data, s"$i $output $data")
    }
    true
  }

  val testGetDefaultArrayString = Property.forAll {
    val tiny = TinyMT32.getDefaultArray(5, "tinyMT", 1)
    val c_data = List(
      List[Int](0xd0065635, 0x8d5e606e, 0x35292e8a, 0xf50272e0, 0x8fa1550b, 0x6aaaed21, 0xb0159b78, 0x4adbe52d,
        0x1ef7779f, 0xfea8cab6),
      List[Int](0x98a8d0c4, 0xf0d0d812, 0xd00df952, 0x5f40c35c, 0x11cc9cab, 0x8769ca59, 0xa06f3c6c, 0xe8285f96,
        0x54005bb2, 0x483fec65),
      List[Int](0x9835dd76, 0x82601445, 0xfd38ca2f, 0x4990fca2, 0x250a026c, 0x20045548, 0x3d23916c, 0x63eec834,
        0x104f5606, 0x8dd4ee2f),
      List[Int](0x343a6834, 0x55e090a9, 0xb00c8b76, 0xc8c3738a, 0xeabdb64b, 0x8ed5908f, 0x3f4393b6, 0xd57b28eb,
        0x43725b82, 0x8e1c4a30),
      List[Int](0x5c8e1f49, 0xbca358a9, 0x6189bbae, 0xe3e7eded, 0x6e2fa730, 0xe1a38ddc, 0x4bbb4104, 0x478baaa4,
        0x521237a7, 0xbfcd41ad)
    )

    c_data.zipWithIndex.foreach { case (data, i) =>
      val output = (0 until data.size).map { _ =>
        val temp = tiny(i).nextInt
        tiny(i) = temp._1
        temp._2
      }
      assert(output == data, i)
    }
    true
  }

  val testSeedArray = Property.forAll {
    var tiny = TinyMT32.getDefault(1)
    val ar = Array[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
    val c_data = List(0x7c14361a, 0x41b450a8, 0xfecc94ae, 0xd19693cd, 0x1d8b3ef7, 0xb5c88b3f, 0x45c79c71, 0xc8465811,
      0x10fcf027, 0x4e3317e5)
    tiny = tiny.setSeed(ar)

    val output = (0 until c_data.length).map { _ =>
      val temp = tiny.nextInt
      tiny = temp._1
      temp._2
    }
    output == c_data
  }

  val testSetSeedLong = Property.forAll {
    var tiny: TinyMT32 = TinyMT32.getDefault(100)
    val output = new Array[Int](10)
    val c_data = Array[Int](0xeead8165, 0x8715ef2c, 0x23598315, 0x5facb14e, 0x73e28fdf, 0x9826df55, 0x8536bb9c,
      0x0c73d703, 0x7b174b03, 0x7ddb6aaf)

    var i = 0
    while (i < output.length) {
      val temp = tiny.nextInt
      output(i) = temp._2
      tiny = temp._1
      i += 1
    }

    assert(Arrays.equals(output, c_data))
    tiny = TinyMT32.getDefault(0x1000000010000000L)
    val c_data2 = Array[Int](0xfd520eb5, 0x6cdab063, 0x2877a46e, 0x5ed3fb03, 0xc1bb07ea, 0xfb872239, 0xd2e2bade,
      0xa41ea287, 0x263e62d3, 0x94a5d5d1)

    i = 0
    while (i < output.length) {
      val temp = tiny.nextInt
      output(i) = temp._2
      tiny = temp._1
      i += 1
    }

    Arrays.equals(output, c_data2)
  }

  val testSetSeedArray = Property.forAll {
    var tiny = TinyMT32.getDefault().setSeed("abcd")
    val output = new Array[Int](10)
    val c_data = Array[Int](0x92264019, 0x3e048b87, 0x0618bd2a, 0x0b85252c, 0xf720f342, 0x73ac4297, 0x20974815,
      0x8ac7a201, 0x7443b188, 0x0b7c6501)

    var i: Int = 0
    while (i < output.length) {
      val temp = tiny.nextInt
      output(i) = temp._2
      tiny = temp._1
      i += 1
    }

    Arrays.equals(output, c_data)
  }

  val testNextFloat = Property.forAll {
    var tiny = TinyMT32.getDefault().setIntSeed(1)
    val c_data = Array[Float](0.5926336050f, 0.2286207080f, 0.8650363684f, 0.5558921099f, 0.8360951543f, 0.8895159960f,
      0.4922972322f, 0.5113201141f, 0.6480515003f, 0.1780070662f)

    var i = 0
    while (i < c_data.length) {
      val temp = tiny.nextFloat
      tiny = temp._1
      assert(math.abs(temp._2 - c_data(i)) < 0.00001, i)
      i += 1
    }
    true
  }

  val testNextDouble = Property.forAll {
    var tiny = TinyMT32.getDefault().setIntSeed(1)
    val c_data = Array[Double](0.5926336137, 0.8650363501, 0.8360951594, 0.4922972448, 0.6480514799, 0.1497518912,
      0.2052537946, 0.8592087870, 0.6802024140, 0.9528347664)

    var i = 0
    while (i < c_data.length) {
      val temp = tiny.nextDouble
      tiny = temp._1
      assert(math.abs(temp._2 - c_data(i)) < 0.00000001, i)
      i += 1
    }
    true
  }

  val testgetThreadLocal = Property.forAll {
    var tiny: TinyMT32 = TinyMT32.getThreadLocal(0)
    assert("80227acb382d7b47f3714bd1223bedaf" == tiny.getCharacteristic)
    tiny = TinyMT32.getThreadLocal(1)
    assert("db46f27d546507bdf3445acd188fa8a3" == tiny.getCharacteristic)
    tiny = TinyMT32.getThreadLocal(-1)
    "db46f27d546507bdf3445acd188fa8a3" == tiny.getCharacteristic
  }
}
