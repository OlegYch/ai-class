package hw_6

import utils.SymbolicVariables

object hw_6_4 extends App with SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal

  def norm(vs: Variable*) = {
    "alpha" =: bd(1) / (vs.map(_.bd).sum)
    vs map (v => v =: v.bd * "alpha")
  }

  norm(
    "p(a')" =: "p(A|a)" =: bd(0.8)
    , "p(b')" =: "p(A|b)" =: bd(0.2)
    , "p(c')" =: "p(A|c)" =: bd(0.2)
  )
}