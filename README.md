# scalaprops

[![Build Status](https://travis-ci.org/scalaprops/scalaprops.svg?branch=master)](https://travis-ci.org/scalaprops/scalaprops)

property based testing library for Scala

### features
- real `scala.FunctionN` generators using [`Cogen`](gen/src/main/scala/scalaprops/Cogen.scala) (aka [CoArbitrary](https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Arbitrary.html#t:CoArbitrary) in QuickCheck). scalaprops can generate not only constant Functions
- flexible parameter settings for each test
- timeout
- flexible law checking like [discipline](https://github.com/typelevel/discipline)
 - discipline uses only `String` for test id. but scalaprops can use other than `String`
- scalaz integration
 - laws for scalaz typeclasses
 - [`Gen`](gen/src/main/scala/scalaprops/Gen.scala) and [`Cogen`](gen/src/main/scala/scalaprops/Cogen.scala) instances of scalaz datatypes
- immutable random number generator
 - scalaprops does not use `scala.util.Random` because `scala.util.Random` is mutable
 - default implementation is [Mersenne Twister](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html)

### latest stable version

```scala
testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false // currently, does not support parallel execution

libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.1.14" % "test"
```

```scala
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.1.14" % "test"
```

or you can use [sbt plugin](https://github.com/scalaprops/sbt-scalaprops)

- [API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/scalaprops/scalaprops-all_2.11/0.1.14/scalaprops-all_2.11-0.1.14-javadoc.jar/!/index.html)
- [sxr](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/scalaprops/scalaprops-all_2.11/0.1.14/scalaprops-all_2.11-0.1.14-sxr.jar/!/index.html)


### snapshot version

```scala
resolvers += Opts.resolver.sonatypeSnapshots

testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false

libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.1.15-SNAPSHOT" % "test"
```

```scala
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.1.15-SNAPSHOT" % "test"
```


- [API Documentation](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/scalaprops/scalaprops-all_2.11/0.1.15-SNAPSHOT/scalaprops-all_2.11-0.1.15-SNAPSHOT-javadoc.jar/!/index.html)
- [sxr](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/scalaprops/scalaprops-all_2.11/0.1.15-SNAPSHOT/scalaprops-all_2.11-0.1.15-SNAPSHOT-sxr.jar/!/index.html)


![screencast](https://raw.githubusercontent.com/scalaprops/scalaprops/master/screencast.gif)
