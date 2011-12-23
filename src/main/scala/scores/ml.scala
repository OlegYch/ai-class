package scores


import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;

import scalala.library.Library._
import scores.analyze.{Score, Offer}
import scalala.tensor.domain.IndexDomain
;

class ml(scores: Seq[Score]) {
  val offers = scores.map(Offer(_))
  val normalized = normalize(DenseMatrix(offers.map(_.x): _*))
  val ys = DenseMatrix(DenseVector(offers.map(_.y): _*).asRow)
  val m = normalized.numRows
  val withIntercept = {
    DenseMatrix.horzcat(DenseMatrix.ones[Double](m, 1), normalized)
  }

  type mt = Matrix[Double]
  def normalize(x: mt): mt = {
    val x_norm: mt = x - DenseMatrix(Seq.fill(x.numRows)(mean(x, Axis.Vertical).asRow): _*)
    val sigma = DenseVector((0 until x_norm.numCols).map(c => x_norm(::, c).stddev): _*)
    x_norm :/ DenseMatrix(Seq.fill(x.numRows)(sigma.asRow): _*)
  }
  var theta = DenseVectorRow[Double](IndexDomain(normalized.numCols))
  val iters = 10000
  val alpha = 0.05
  (0 to iters).foreach {_ =>
    val h = DenseMatrix(theta) * normalized.t
    val deltaTheta: mt = h - ys
    val cost = (deltaTheta :^ 2).sum * (1.0 / 2 / m);
    println(cost)
    theta = theta.mapPairs((i: Int, d: Double) => theta(i) - alpha / m * (deltaTheta :* DenseMatrix(normalized(::, i)
      .asRow)).sum)
  }
  println(theta)
}
