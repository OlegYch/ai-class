package hw_7

import scalala.tensor.dense._
import scalala.tensor.domain.DomainException
import utils.{MatrixSymbolicVariables, BigDecimalSymbolicVariables}


object hw_7_3 extends App with MatrixSymbolicVariables {
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