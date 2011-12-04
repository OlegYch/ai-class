package hw_7

import utils.{FunctionSystems, BigDecimalSymbolicVariables}


object hw_7_4 extends App with BigDecimalSymbolicVariables with FunctionSystems {
  type function = StereoDistance
  case class StereoDistance(
                             //                                    Z: Dim = (p: perspectiveProjection) => p.X * p.f /
                             // p.x
                             ) extends FunctionSystem

  def m = bd(1)
  def mm = bd(1000)
  //  "X m" =: perspectiveProjection(Z = 300 * m, f = 100 * mm, x = 1 * mm).X / m
  //  "Z m" =: perspectiveProjection(X = 2 * m, f = 40 * mm, x = 1 * mm).Z / m
  //  "f mm" =: perspectiveProjection(X = 20 * m, Z = 400 * m, x = 1 * mm).f / mm
  //  "x mm" =: perspectiveProjection(X = 10 * m, Z = 100 * m, f = 10 * mm).x / mm
}