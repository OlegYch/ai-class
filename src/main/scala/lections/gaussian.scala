package lections

import utils.SymbolicVariables

object gaussian extends App with SymbolicVariables {
  override type bd = BigDecimal
  val data = Seq[bd](3, 9, 9, 3)
  //  val data: Seq[bd] = Seq(3, 4, 5, 6, 7)
  val m: bd = data.size
  "mu" =: 1 / m * (data.sum)
  "sigma^2" =: 1 / m * (data.map(_ - 'mu).map(_.pow(2)).sum)
}