package hw_6

import utils.SymbolicVariables

object hw_6_5 extends App with SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal

  def norm(vs: Variable*) = {
    "alpha" =: bd(1) / (vs.map(_.bd).sum)
    vs map (v => v =: v.bd * "alpha")
  }

  val vals = for (l <- "ab"; n <- "12") yield l.toString + n
  norm(
    vals.map {case v@"a2" => v =: bd(0) case v => v =: bd(1)}: _*
  )

  case class Transition(from: String, to: String) {
    val transition = from + "->" + to

    def apply(v: bd) = {transition =: v; this}
  }

  val transitions = for (from <- vals; to <- vals) yield {
    Transition(from, to)(if ((from + to toSet).size == 3) bd(0.5) else bd(0))
  }
  for (to <- vals) {
    (to + "'") =: transitions.filter(_.to == to).map(t => t.transition.bd * t.from).sum
  }
}