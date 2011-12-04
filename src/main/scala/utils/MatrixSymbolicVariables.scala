package utils

import scalala.tensor.dense.DenseMatrix

trait MatrixSymbolicVariables extends SymbolicVariables {
  type b = BigDecimal
  type bd = DenseMatrix[b]
  type mbd = bd
  def bd(s: Int*) = s.map(BigDecimal(_)).toArray
  implicit object ScalarBigDecimal extends utils.ScalarBigDecimal
}