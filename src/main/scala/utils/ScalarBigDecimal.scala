package utils

import scalala.scalar.Scalar
import scalala.collection.sparse.DefaultArrayValue

class ScalarBigDecimal extends Scalar[BigDecimal] {
  type b = BigDecimal
  def zero = 0;
  def one = 1;
  def nan = throw new ArithmeticException("Operation resulted in integer-valued NaN");
  def ==(a: b, b: b) = a == b;
  def !=(a: b, b: b) = a != b;
  def >(a: b, b: b) = a > b;
  def >=(a: b, b: b) = a >= b;
  def <(a: b, b: b) = a < b;
  def <=(a: b, b: b) = a <= b;
  def +(a: b, b: b) = a + b;
  def -(a: b, b: b) = a - b;
  def *(a: b, b: b) = a * b;
  def /(a: b, b: b) = a / b;
  def norm(a: b) = toDouble(if (a < 0) -a else a);
  def toDouble(a: b) = a.toDouble;
  def isNaN(a: b) = false;
  val manifest = implicitly[ClassManifest[b]];
  val defaultArrayValue = implicitly[DefaultArrayValue[b]];
}
