package utils

trait BigDecimalSymbolicVariables extends SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal

  implicit def toV(i: Int) = new {val v = BigDecimal(i)}

  implicit def toV(i: Double) = new {val v = BigDecimal(i)}

  implicit def toBD(i: Int) = BigDecimal(i)

  implicit def toBD(i: Double) = BigDecimal(i)
}