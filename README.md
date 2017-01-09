# scalaprops

[![Build Status](https://travis-ci.org/scalaprops/scalaprops.svg?branch=master)](https://travis-ci.org/scalaprops/scalaprops)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.13.svg)](https://www.scala-js.org)

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

### latest stable version

```scala
testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false // currently, does not support parallel execution

libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.3.5" % "test"
```

```scala
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.3.5" % "test"
```

or you can use [sbt plugin](https://github.com/scalaprops/sbt-scalaprops)

- [API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/scalaprops/scalaprops-all_2.11/0.3.5/scalaprops-all_2.11-0.3.5-javadoc.jar/!/index.html)
- [sxr](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/scalaprops/scalaprops-all_2.11/0.3.5/scalaprops-all_2.11-0.3.5-sxr.jar/!/index.html)


### snapshot version

```scala
resolvers += Opts.resolver.sonatypeSnapshots

testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false

libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.3.5-SNAPSHOT" % "test"
```

```scala
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.3.5-SNAPSHOT" % "test"
```


- [API Documentation](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/scalaprops/scalaprops-all_2.11/0.3.5-SNAPSHOT/scalaprops-all_2.11-0.3.5-SNAPSHOT-javadoc.jar/!/index.html)
- [sxr](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/scalaprops/scalaprops-all_2.11/0.3.5-SNAPSHOT/scalaprops-all_2.11-0.3.5-SNAPSHOT-sxr.jar/!/index.html)


![screencast](https://raw.githubusercontent.com/scalaprops/scalaprops/master/screencast.gif)

### for scalaz 7.1.x

<https://github.com/scalaprops/scalaprops/tree/0.1.x>
