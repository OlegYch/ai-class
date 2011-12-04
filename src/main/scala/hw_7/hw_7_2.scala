package hw_7

import scalala.scalar._
import scalala.tensor.dense._
import utils.{BigDecimalSymbolicVariables, SymbolicVariables}
import scalala.collection.sparse.DefaultArrayValue
import scalala.tensor.domain.DomainException


object hw_7_2 extends App with SymbolicVariables {
  type b = BigDecimal
  type bd = DenseMatrix[b]
  type mbd = bd
  implicit object ScalarBD extends Scalar[b] {
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

  def bd(s: Int*) = s.map(BigDecimal(_)).toArray
  "I" =: DenseMatrix(bd(2, 0, 2), bd(4, 100, 102), bd(2, 4, 2))
  "g" =: DenseMatrix(bd(-1, 0, 1))
  def get(m: bd, x: Int, y: Int): BigDecimal = {
    try m(row = y, col = x)
    catch {case e: DomainException => 0}
  }
  "I'" =: 'I.mapTriples {
    case (row, col, v) => 'g.mapTriples {
      case (_, u, gu) => {
        println("-----------")
        val m = 'I: mbd
        println(m)
        println("========")
        new BigDecimalSymbolicVariables {
          'x =: bd(col)
          'u =: bd(u)
          "x'" =: 'x - 1 + u
          'y =: bd(row)
          "Ix'y" =: get(m, "x'".bd.toInt, 'y.toInt)
          'gu =: gu
          val s = ('s =: "Ix'y" * gu).bd
        }.s
      }
    }.sum.abs
  }
}