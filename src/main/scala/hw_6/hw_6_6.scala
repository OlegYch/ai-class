package hw_6

import utils.SymbolicVariables

object hw_6_6 extends App with SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal

  import lections.mixed_strategy._

  lections.mixed_strategy.main(Array())

  solveTheGame(List(4, -5, -3, 5))
}