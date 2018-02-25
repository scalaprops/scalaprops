# scalaprops

[![Build Status](https://travis-ci.org/scalaprops/scalaprops.svg?branch=master)](https://travis-ci.org/scalaprops/scalaprops)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.14.svg)](https://www.scala-js.org)
[![scaladoc](https://javadoc-badge.appspot.com/com.github.scalaprops/scalaprops-all_2.12.svg?label=scaladoc)](https://javadoc-badge.appspot.com/com.github.scalaprops/scalaprops-all_2.12/scalaprops/index.html?javadocio=true)

property based testing library for Scala

### features
- real `scala.FunctionN` generators using [`Cogen`](gen/src/main/scala/scalaprops/Cogen.scala) (aka [CoArbitrary](https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Arbitrary.html#t:CoArbitrary) in QuickCheck). scalaprops can generate not only constant Functions
- flexible parameter settings for each test( [ScalaCheck doesn't have this feature](https://github.com/rickynils/scalacheck/issues/120) )
- timeout as soon as possible
- flexible law checking like [discipline](https://github.com/typelevel/discipline)
 - discipline uses only `String` for test id. but scalaprops can use other than `String`
- scalaz integration
 - laws for scalaz typeclasses
 - [`Gen`](gen/src/main/scala/scalaprops/Gen.scala) and [`Cogen`](gen/src/main/scala/scalaprops/Cogen.scala) instances of scalaz datatypes
- immutable random number generator
 - scalaprops does not use `scala.util.Random` because `scala.util.Random` is mutable
 - default implementation is [Mersenne Twister](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html) (JVM) or [Tiny Mersenne Twister](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/TINYMT/) (Scala.js)
- [Scala.js](https://www.scala-js.org/) support
- [scala-native](http://scala-native.org) support. see <https://github.com/scalaprops/scalaprops-native-example>
- [deterministic testing](#deterministic-testing)

### latest stable version

[please use sbt plugin because there are some convenient features.](https://github.com/scalaprops/sbt-scalaprops)


<details><summary>setup without sbt plugin</summary>

```scala
testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false // currently, does not support parallel execution

libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.5.3" % "test"
```

```scala
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.5.3" % "test"
```

</details>


- [API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/scalaprops/scalaprops-all_2.12/0.5.3/scalaprops-all_2.12-0.5.3-javadoc.jar/!/scalaprops/index.html)
- [sxr](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/scalaprops/scalaprops-all_2.12/0.5.3/scalaprops-all_2.12-0.5.3-sxr.jar/!/index.html)


### snapshot version

<details><summary>setup without sbt plugin</summary>

```scala
resolvers += Opts.resolver.sonatypeSnapshots

testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false

libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.5.4-SNAPSHOT" % "test"
```

```scala
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.5.4-SNAPSHOT" % "test"
```

</details>

- [API Documentation](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/scalaprops/scalaprops-all_2.12/0.5.4-SNAPSHOT/scalaprops-all_2.12-0.5.4-SNAPSHOT-javadoc.jar/!/scalaprops/index.html)
- [sxr](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/scalaprops/scalaprops-all_2.12/0.5.4-SNAPSHOT/scalaprops-all_2.12-0.5.4-SNAPSHOT-sxr.jar/!/index.html)


![screencast](screencast.gif)

### deterministic testing

Scalaprops emitting which seed it started with during a failing test, and providing an interface `--seed=<value>` for re-running the failing test with the same seed.

![deterministic-testing](deterministic-testing.gif)

### for scalaz 7.1.x

<https://github.com/scalaprops/scalaprops/tree/0.1.x>
