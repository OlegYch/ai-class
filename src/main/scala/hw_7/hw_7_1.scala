package hw_7

import utils.BigDecimalSymbolicVariables

object hw_7_1 extends App with BigDecimalSymbolicVariables {
  type Dim = Either[bd, pp => bd]
  implicit def toLeft(b: bd) = Left(b)
  implicit def toRight(b: pp => bd) = Right(b)
  var currentProjection: pp = _
  implicit def fromDim(b: Dim): bd = b match {case Left(b) => b; case Right(b) => b(currentProjection)}
  case class pp(
                 x: Dim = (p: pp) => p.X * p.f / p.Z,
                 f: Dim = (p: pp) => p.x * p.Z / p.X,
                 X: Dim = (p: pp) => p.x * p.Z / p.f,
                 Z: Dim = (p: pp) => p.X * p.f / p.x
                 ) {
    currentProjection = this
  }

  def m = bd(1)
  def mm = bd(1000)
  "X m" =: pp(Z = 300 * m, f = 100 * mm, x = 1 * mm).X / m
  "Z m" =: pp(X = 2 * m, f = 40 * mm, x = 1 * mm).Z / m
  "f mm" =: pp(X = 20 * m, Z = 400 * m, x = 1 * mm).f / mm
  "x mm" =: pp(X = 10 * m, Z = 100 * m, f = 10 * mm).x / mm
}