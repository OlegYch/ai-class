package hw_7

import utils.{FunctionSystems, BigDecimalSymbolicVariables}

object hw_7_4 extends App with BigDecimalSymbolicVariables with FunctionSystems {
  type function = StereoDistance
  case class StereoDistance(
                             Z: Dim = (p: function) => p.f * p.B / p.deltaX,
                             x1: Dim = undef,
                             x2: Dim = undef,
                             B: Dim = (p: function) => p.Z * p.deltaX / p.f,
                             f: Dim = (p: function) => p.Z * p.deltaX / p.B,
                             deltaX: Dim = (p: function) => {
                               val fromX = for (x2 <- p.x2.?; x1 <- p.x1.?) yield (x2 - x1)
                               fromX getOrElse (p.f * p.B / p.Z)
                             }
                             ) extends FunctionSystem

  def m = bd(1)
  def cm = bd(100)
  def mm = bd(1000)
  "Z cm" =: StereoDistance(f = 8 * mm, x1 = -1 * mm, x2 = 3 * mm, B = 20 * cm).Z / cm
  "deltaX mm" =: StereoDistance(Z = 10 * m, f = 30 * mm, B = 1 * m).deltaX / mm
  "f mm" =: StereoDistance(Z = 100 * m, deltaX = 1 * mm, B = 0.5 * m).f / mm
  println("==============")
  "Z m" =: StereoDistance(deltaX = 4 * mm, f = 40 * mm, B = 0.1 * m).Z / m
  "B m" =: StereoDistance(deltaX = 0.05 * mm, f = 50 * mm, Z = 100 * m).B / m
  "f mm" =: StereoDistance(deltaX = 0.1 * mm, B = 0.2 * m, Z = 50 * m).f / mm
  "deltaX mm" =: StereoDistance(f = 200 * mm, B = 1 * m, Z = 50 * m).deltaX / mm
}