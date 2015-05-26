# scalaprops

[![Build Status](https://secure.travis-ci.org/scalaprops/scalaprops.png)](http://travis-ci.org/scalaprops/scalaprops)

property based testing library for Scala

### features
- real `scala.FunctionN` generators using [`Cogen`](core/src/main/scala/scalaprops/Cogen.scala) (aka [CoArbitrary](https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-Arbitrary.html#t:CoArbitrary) in QuickCheck). scalaprops can generate not only constant Functions
- flexible parameter settings for each tests
- timeout
- flexible law checking like [discipline](https://github.com/typelevel/discipline)
 - discipline uses only `String` for test id. but scalaprops can use other than `String`
- scalaz integration
 - laws for scalaz typeclasses
 - [`Gen`](core/src/main/scala/scalaprops/Gen.scala) and [`Cogen`](core/src/main/scala/scalaprops/Cogen.scala) instances of scalaz datatypes
- immutable random number generator
 - scalaprops does not use `scala.util.Random` because `scala.util.Random` is mutable


### latest stable version

```scala
testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false // currently, does not support parallel execution

libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.1.5" % "test"
```

```scala
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.1.5" % "test"
```

- [API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/scalaprops/scalaprops-all_2.11/0.1.5/scalaprops-all_2.11-0.1.5-javadoc.jar/!/index.html)
- [sxr](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/scalaprops/scalaprops-all_2.11/0.1.5/scalaprops-all_2.11-0.1.5-sxr.jar/!/index.html)


### snapshot version

```scala
resolvers += Opts.resolver.sonatypeSnapshots

testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false

libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.1.6-SNAPSHOT" % "test"
```

```scala
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.1.6-SNAPSHOT" % "test"
```


- [API Documentation](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/scalaprops/scalaprops-all_2.11/0.1.6-SNAPSHOT/scalaprops-all_2.11-0.1.6-SNAPSHOT-javadoc.jar/!/index.html)
- [sxr](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/scalaprops/scalaprops-all_2.11/0.1.6-SNAPSHOT/scalaprops-all_2.11-0.1.6-SNAPSHOT-sxr.jar/!/index.html)


![screencast](https://raw.githubusercontent.com/scalaprops/scalaprops/master/screencast.gif)
