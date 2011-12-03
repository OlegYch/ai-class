package hw_7

import utils.BigDecimalSymbolicVariables


object hw_7_1 extends App with BigDecimalSymbolicVariables {
  implicit def toUnit(b: Int) = new Unit(b)

  case class Unit(b: BigDecimalSymbolicVariables#bd) {
    def m = b
    def mm = b * 1000
  }

  trait pp {
    def x: bd = X * f / Z
    def f: bd = x * Z / X
    def X: bd = x * Z / f
    def Z: bd = X * f / x
  }

  "X m" =: new pp {
    override def Z = 300 m
    override def f = 100 mm
    override def x = 1 mm
  }.X / (1 m)
  "Z m" =: new pp {
    override def X = 2 m
    override def f = 40 mm
    override def x = 1 mm
  }.Z / (1 m)
  "f mm" =: new pp {
    override def X = 20 m
    override def Z = 400 m
    override def x = 1 mm
  }.f / (1 mm)
  "x mm" =: new pp {
    override val X = 10 m
    override def Z = 100 m
    override def f = 10 mm
  }.x / (1 mm)
}