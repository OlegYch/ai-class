package hw_7

import utils.{FunctionSystems, BigDecimalSymbolicVariables}

object hw_7_4 extends App with BigDecimalSymbolicVariables with FunctionSystems {
  type function = StereoDistance
  case class StereoDistance(
                             Z: Dim = (p: function) => p.f * p.B / (p.x2 - p.x1),
                             x1: Dim,
                             x2: Dim,
                             B: Dim,
                             f: Dim
                             ) extends FunctionSystem
  def cm = bd(100)
  def mm = bd(1000)
  "Z cm" =: StereoDistance(f = 8 * mm, x1 = -1 * mm, x2 = 3 * mm, B = 20 * cm).Z / cm
}