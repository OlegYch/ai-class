import scalala.scalar._;

import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._
import scalala.tensor.{Matrix, ::}

object gaussian_multi extends App {
  type bd = Double
  val bd = Double
  val data = DenseMatrix((3.0, 4.0, 5.0, 6.0, 7.0), (8.0,7.0,5.0,3.0,2.0)).t
  println(data)
  val m = data.numRows
  val k = data.numCols
  val mu = sum(data) :/ m
  println("mu=" + mu)
  val muFilled = DenseMatrix.vertcat(Seq.fill(m.toInt)(DenseMatrix(mu)): _*)
  val diff = data - muFilled
  val sigma = diff.t * diff / m
  println("sigma=" + sigma)
  val s = DenseMatrix.zeros[Int](k, k).mapPairs {
    (p, _) => p match {
      case (j, k) =>
        (0 until m).map(i => (data(i, j) - mu(j)) * (data(i, k) - mu(k))).sum / m
    }
  }
  println("sigma2=" + s)
}                           