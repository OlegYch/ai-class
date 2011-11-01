object regression extends App {
  val bd = BigDecimal
  type bd = BigDecimal

  val ys = List(3, 6, 7, 8, 11).map(bd.apply)
  val xs = List(0, 1, 2, 3, 4).map(bd.apply)
  //  val ys = List(2, 5, 5, 8).map(bd.apply)
  //  val xs = List(2, 4, 6, 8).map(bd.apply)
  val m = bd(ys.size)
  lazy val w0 = ys.sum / m - w1 * xs.sum / m
  lazy val w1 =
    (m * ys.zip(xs).map {case (y, x) => y * x}.sum - xs.sum * ys.sum) /
      (m * xs.map(_.pow(2)).sum - xs.sum.pow(2))
  println(w0)
  println(w1)

  def f(x: bd) = x * w1 + w0

  println(xs.map(f))
}