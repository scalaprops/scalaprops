package scalaprops

abstract class GenInstances1 extends GenInstances {

  implicit def javaEnumGen[A <: java.lang.Enum[A]](implicit A: reflect.ClassTag[A]): Gen[A] = {
    val array = A.runtimeClass.getEnumConstants.asInstanceOf[Array[A]]
    Gen.choose(0, array.length - 1).map(array)
  }

}
