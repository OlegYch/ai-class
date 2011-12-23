package scores


import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;

import scalala.library.Numerics
import scores.analyze.{Score, Offer}
import scalala.tensor.domain.IndexDomain
import scalala.library.Library._
;

class ml(scores: Seq[Score]) {
  val offers = scores.map(Offer(_))
  val trainingSet = DenseMatrix(offers.map(_.x): _*)
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
  var theta = DenseVectorRow[Double](IndexDomain(xs.numCols))
  val iters = 500
  val alpha = 0.1
  def hTest(x: mt): mt = h(withIntercept(normalized.norm(x)))
  def h: mt = h(xs)
  private def h(xs: mt): mt = (DenseMatrix(theta) * xs.t).mapValues(Numerics.sigmoid)
  (0 to iters).foreach {_ =>
    val hh = h
    val deltaTheta: mt = hh - ys
    val cost = (-ys :* hh.mapValues(log) - (-ys :+ 1) :* (-hh :+ 1).mapValues(log)).sum * (1.0 / m)
    println(cost)
    theta = theta.mapPairs((i: Int, d: Double) =>
      theta(i) - alpha / m * (deltaTheta :* DenseMatrix(xs(::, i).asRow)).sum)
  }
  println(theta)
}
