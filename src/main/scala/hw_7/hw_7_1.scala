package hw_7

import utils.{FunctionSystems, BigDecimalSymbolicVariables}
import scala.math.BigDecimal.RoundingMode


object hw_7_1 extends App with BigDecimalSymbolicVariables with FunctionSystems {
  def rnd = (math.random * m).setScale(2, RoundingMode.HALF_UP) * 10
  override def default = (v) => () => v =: rnd

  type function = perspectiveProjection
  case class perspectiveProjection(
                                    x: Dim = (p: perspectiveProjection) => p.X * p.f / p.Z,
                                    f: Dim = (p: perspectiveProjection) => p.x * p.Z / p.X,
                                    X: Dim = (p: perspectiveProjection) => p.x * p.Z / p.f,
                                    Z: Dim = (p: perspectiveProjection) => p.X * p.f / p.x
                                    ) extends FunctionSystem
  def m = bd(1)
  def mm = bd(1000)
  "X m" =: perspectiveProjection(Z = 300 * m, f = 100 * mm, x = 1 * mm).X / m
  "Z m" =: perspectiveProjection(X = 2 * m, f = 40 * mm, x = 1 * mm).Z / m
  "f mm" =: perspectiveProjection(X = 20 * m, Z = 400 * m, x = 1 * mm).f / mm
  "x mm" =: perspectiveProjection(X = 10 * m, Z = 100 * m, f = 10 * mm).x / mm
}