package hw_6

import utils.SymbolicVariables

object hw_6_3 extends App with SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal

  "p(a0)" =: "p(b0)" =: bd(0.5)
  "p(a|a)" =: "p(b|a)" =: "p(b|b)" =: "p(a|b)" =: bd(0.5)
  "p(x|a)" =: bd(0.1)
  "p(y|a)" =: bd(0.9)
  "p(x|b)" =: bd(0.8)
  "p(y|b)" =: bd(0.2)
  "p(a)" =: "p(b)" =: bd(0.5)
  "p(a1)" =: "p(b1)" =: bd(0.5)
  "p(x0)" =: "p(x|a)".bd * "p(a0)" + "p(x|b)".bd * "p(b0)"
  "p(x1)" =: "p(x|a)".bd * "p(a1)" + "p(x|b)".bd * "p(b1)"
  "p(x0x1)" =: "p(x0)".bd * "p(x1)"

  "p(x0|a0)" =: bd(1)

  "p(x)" =: "p(x|a)".bd * "p(a)" + "p(x|b)".bd * "p(b)"

  def norm(a: Variable, b: Variable) = {
    "alpha" =: bd(1) / (a.bd + b.bd)
    a =: a.bd * "alpha"
    b =: b.bd * "alpha"
  }

  "p(a0|x0)" =: "p(x|a)".bd * "p(a0)" / "p(x0)"
  "p(a0|x0)" =: "p(x|a)".bd * "p(a0)"
  "p(b0|x0)" =: "p(x|b)".bd * "p(b0)"
  norm("p(a0|x0)", "p(b0|x0)")
  "p(a1|x0)" =: "p(x|a)".bd * "p(a1)" / "p(x0)"
  "p(b1|x0)" =: "p(x|b)".bd * "p(b1)" / "p(x0)"
  "p(a1|x0)" =: "p(a|a)".bd * "p(a0|x0)" + "p(a|b)".bd * "p(b0|x0)"
  "p(b1|x0)" =: "p(b|a)".bd * "p(a0|x0)" + "p(b|b)".bd * "p(b0|x0)"
  norm("p(a1|x0)", "p(b1|x0)")
  "p(a1|x0x1)" =: "p(x|a)".bd * "p(a1|x0)"
  "p(b1|x0x1)" =: "p(x|b)".bd * "p(b1|x0)"
  norm("p(a1|x0x1)", "p(b1|x0x1)")
}