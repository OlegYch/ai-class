package lections

import utils.SymbolicVariables

class regression extends SymbolicVariables {
  lazy val bd = BigDecimal
  type bd = BigDecimal

  lazy val ys = List(3, 6, 7, 8, 11).map(bd.apply)
  lazy val xs = List(0, 1, 2, 3, 4).map(bd.apply)
  //  val ys = List(2, 5, 5, 8).map(bd.apply)
  //  val xs = List(2, 4, 6, 8).map(bd.apply)
  lazy val m = bd(ys.size)
  lazy val w0 = ys.sum / m - w1 * xs.sum / m
  lazy val w1 =
    (m * ys.zip(xs).map {case (y, x) => y * x}.sum - xs.sum * ys.sum) /
      (m * xs.map(_.pow(2)).sum - xs.sum.pow(2))
  'w0 =: w0
  'w1 =: w1

  def f(x: bd) = x * w1 + w0

  xs.map(x => "f(%s)".format(x) =: f(x))
}

object regression extends regression with App {

}