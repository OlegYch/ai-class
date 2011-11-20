package midterm

import lections.regression

object midterm_10 extends regression with App {
  override lazy val ys = List(2, 5.2, 6.8, 8.4, 14.8).map(BigDecimal.apply)
  override lazy val xs = List(1, 3, 4, 5, 9).map(BigDecimal.apply)
}