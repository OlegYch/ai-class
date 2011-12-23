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
  val m = normalized.numRows
  def withIntercept(normalized: mt) = {
    DenseMatrix.horzcat(DenseMatrix.ones[Double](normalized.numRows, 1), normalized)
  }

  type mt = Matrix[Double]
  def normalize(x: mt): mt = {
    val x_norm: mt = x - DenseMatrix(Seq.fill(x.numRows)(mean(x, Axis.Vertical).asRow): _*)
    val sigma = DenseVector((0 until x_norm.numCols).map(c => x_norm(::, c).stddev): _*)
    x_norm :/ DenseMatrix(Seq.fill(x.numRows)(sigma.asRow): _*)
  }
  val xs = withIntercept(normalized)
  var theta = DenseVectorRow[Double](IndexDomain(xs.numCols))
  val iters = 100
  val alpha = 0.05
  def h(x: mt): mt = DenseMatrix(theta) * withIntercept(DenseMatrix(normalize(DenseMatrix
    .vertcat(x, trainingSet))(0, ::))).t
  def h: mt = (DenseMatrix(theta) * xs.t).mapValues(Numerics.sigmoid)
  (0 to iters).foreach {_ =>
    val hh = h
    val deltaTheta: mt = hh - ys
    val cost = (-ys :* hh.mapValues(log) - (-ys :+ 1) :* (-hh :+ 1).mapValues(log)).sum * (1.0 / m);
    println(cost)
    theta = theta.mapPairs((i: Int, d: Double) =>
      theta(i) - alpha / m * (deltaTheta :* DenseMatrix(xs(::, i).asRow))
        .sum)
  }
  println(theta)
}
