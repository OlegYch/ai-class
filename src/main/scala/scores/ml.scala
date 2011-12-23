package scores


import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._

import scalala.library.Numerics
import scores.analyze.{Score, Offer}
import scalala.library.Library._
import scalala.library.Plotting._
import scala.math.BigDecimal.RoundingMode

@EnhanceStrings
class ml(scores: Seq[Score]) {
  val offers = scores.map(Offer(_))
  val trainingSet: mt = DenseMatrix(offers.map(_.x): _*)
  val normalized = normalize(trainingSet)
  val ys = DenseMatrix(DenseVector(offers.map(_.y): _*).asRow)
  val m = scores.size
  def withIntercept(normalized: mt) = {
    DenseMatrix.horzcat(DenseMatrix.ones[Double](normalized.numRows, 1), normalized)
  }

  type mt = Matrix[Double]
  def normalize(x: mt) = new {
    val mu = mean(x, Axis.Vertical).asRow
    def x_norm(x: mt): mt = x - DenseMatrix(Seq.fill(x.numRows)(mu): _*)
    val x_norm: mt = x_norm(x)
    val sigma = DenseVector((0 until x_norm.numCols).map(c => x_norm(::, c).stddev): _*)
    def norm(x: mt): mt = x_norm(x) :/ DenseMatrix(Seq.fill(x.numRows)(sigma.asRow): _*)
    val norm: mt = norm(x)
  }
  val xs = withIntercept(normalized.norm)
  var theta = DenseVector(Seq.fill(xs.numCols)(0.0): _*).asRow
  val lambda = DenseVector(Seq.fill(xs.numCols)(1.0): _*).asRow
  lambda(0) = 0
  val iters = 500
  var alpha = 5.0
  var lastCost = 0.0
  def hTest(x: mt): mt = h(withIntercept(normalized.norm(x)))
  def h: mt = h(xs)
  private def h(xs: mt): mt = (DenseMatrix(theta) * xs.t).mapValues(Numerics.sigmoid)
  val costs = (0 to iters).map {i =>
    val hh = h
    val positiveCost: mt = -ys :* hh.mapValues(log)
    val negativeCost: mt = (-ys :+ 1) :* (-hh :+ 1).mapValues(log)
    val cost: Double = (positiveCost - negativeCost).sum / m + (lambda :* (theta :^ 2)).sum / (2 * m)
    if (!cost.isInfinite) {
      println("Current cost " + BigDecimal(cost).setScale(8, RoundingMode.HALF_UP))
      if (lastCost < cost) {
        alpha *= 0.9
        lambda /= (1 / 0.9)
        println("detected increasing cost, decreasing alpha to #alpha and lambda to #lambda")
      }
      lastCost = cost
    }
    val deltaTheta: mt = hh - ys
    theta = theta.mapPairs((i: Int, d: Double) =>
      theta(i) - (alpha / m * (deltaTheta :* DenseMatrix(xs(::, i).asRow)).sum) + lambda(i) * theta(i) / m)
    (i, cost)
  }.filterNot(_._2.isInfinite).toArray
  println("Final weights for features:")
  println((Seq("X0") ++ Offer(scores(0)).features.map(_.label)).mkString("  \t"))
  println(theta.values.toSeq.map(BigDecimal(_).setScale(3, RoundingMode.HALF_UP)).mkString("\t"))
  println("See cost plot, close the window to exit")
  xlabel("Iteration")
  ylabel("Cost")
  plot(costs.map(_._1), costs.map(_._2))
  //  val normal = inv(trainingSet.t * trainingSet) * trainingSet * ys
  //  println(normal)
}
