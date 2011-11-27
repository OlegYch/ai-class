package hw_6

import utils.SymbolicVariables

object hw_6_2 extends App with SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal

  "p(a|a)" =: bd(0.9)
  "p(b|a)" =: bd(0.1)
  "p(b|b)" =: bd(0.5)
  "p(a|b)" =: bd(0.5)

  def `p(a+1)`(initial: bd) = {
    "p(a)" =: initial
    "p(b)" =: bd(1) - "p(a)"
    "p(a+1)" =: "p(a)".bd * "p(a|a)" + "p(b)".bd * "p(a|b)"
  }

  val s = Stream.iterate(bd(0.5))(`p(a+1)` _)
  println(s.zip(s.drop(1)).find(p => p._1 == p._2))
}