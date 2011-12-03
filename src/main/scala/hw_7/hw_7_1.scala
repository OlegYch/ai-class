package hw_7

import utils.BigDecimalSymbolicVariables

object hw_7_1 extends App with BigDecimalSymbolicVariables {
  type Dim = Either[bd, perspectiveProjection => bd]
  implicit def toLeft(b: bd) = Left(b)
  implicit def toRight(b: perspectiveProjection => bd) = Right(b)
  var currentProjection: perspectiveProjection = _
  implicit def fromDim(b: Dim): bd = b match {case Left(b) => b; case Right(b) => b(currentProjection)}
  case class perspectiveProjection(
                                    x: Dim = (p: perspectiveProjection) => p.X * p.f / p.Z,
                                    f: Dim = (p: perspectiveProjection) => p.x * p.Z / p.X,
                                    X: Dim = (p: perspectiveProjection) => p.x * p.Z / p.f,
                                    Z: Dim = (p: perspectiveProjection) => p.X * p.f / p.x
                                    ) {
    currentProjection = this
  }

  def m = bd(1)
  def mm = bd(1000)
  "X m" =: perspectiveProjection(Z = 300 * m, f = 100 * mm, x = 1 * mm).X / m
  "Z m" =: perspectiveProjection(X = 2 * m, f = 40 * mm, x = 1 * mm).Z / m
  "f mm" =: perspectiveProjection(X = 20 * m, Z = 400 * m, x = 1 * mm).f / mm
  "x mm" =: perspectiveProjection(X = 10 * m, Z = 100 * m, f = 10 * mm).x / mm
}