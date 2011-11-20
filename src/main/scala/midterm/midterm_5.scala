package midterm

import utils.SymbolicVariables

object midterm_5 extends App with SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal
  "p(l)" =: "p(!l)" =: bd(0.5)
  "p(hh|!l)" =: ("p(h|!l)" =: bd(0.5)).pow(2)
  "p(hh|l)" =: "p(h|l)" =: bd(1)
  "p(h)" =: "p(h|l)".bd * "p(l)" + "p(h|!l)".bd * "p(!l)"
  "p(hh)" =: "p(hh|l)".bd * "p(l)" + "p(hh|!l)".bd * "p(!l)"
  "p(l|h)" =: "p(h|l)".bd * "p(l)" / "p(h)"
  "p(l|hh)" =: "p(hh|l)".bd * "p(l)" / "p(hh)"
}