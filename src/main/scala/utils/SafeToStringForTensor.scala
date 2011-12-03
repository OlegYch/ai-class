package utils

/**
 * to workaround scalala.tensor.TensorLike#toString(int, scala.Function1<K,java.lang.String>, scala.Function1<V,java.lang.String>)
 * throwing exception when running in intellij console due to Terminal.height returning 1
 */
trait SafeToStringForTensor {
  type Stringable[V] = {
    def toString(maxLines: Int, maxWidth: Int, mkValueString: V => String): String
    def buildMkValueString: V => String
  }

  implicit def withSafeToString[T <: Stringable[_]](value: T) = new SafeToStringWrapper(value)
  implicit def fromSafeToString[T <: Stringable[_]](value: SafeToStringValue[T]) = value.v

  class SafeToStringWrapper[T <: Stringable[_]](value: T) {
    def s = new SafeToStringValue(value)
  }

  class SafeToStringValue[T <: Stringable[_]](value: T) {
    def v = value
    override def toString = value.toString(maxLines = 100, maxWidth = 100, mkValueString = value.buildMkValueString)
  }
}