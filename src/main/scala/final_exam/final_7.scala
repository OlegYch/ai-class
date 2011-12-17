package final_exam

import utils.BigDecimalSymbolicVariables
import org.scalatest.FunSuite

@EnhanceStrings
class final_7 extends FunSuite with BigDecimalSymbolicVariables {
  test("A sampled at least once") {
    val n = 3
    "w(A)" =: 0.2.v
    "w(!A)" =: 1.v - "w(A)"
    "w(#n!A)" =: "w(!A)".bd.pow(n)
    "w(!(#n!A))" =: 1.v - "w(#n!A)"
  }
}