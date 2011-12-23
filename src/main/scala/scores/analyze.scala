package scores

import scala.collection.immutable.TreeMap
import scala.math.BigDecimal.RoundingMode
import scalala.tensor.dense.DenseMatrix

@EnhanceStrings
object analyze extends App {
  val allRows = new extractScores().getAllScoreRows
  val scores = allRows.filter(_.exists(_ != "empty")).map(vals => Score(vals)(allRows(0)))
  println(scores.map(_.valsByName))
  val withOffer = scores.filter(_.gotOffer).sortBy(-_.score)
  println(TreeMap(withOffer.groupBy(_.age.value).toSeq: _*).map {
    case (_, scores) => scores.map(s => (s.score.value, s.valsByNameNotEmpty)).mkString("\n")
  }.mkString("\n"))
  println(withOffer.size)
  val ml = new ml(scores)
  def test(scores: IndexedSeq[Score]) {
    println(scores.view.map(Offer(_)).map(o => (ml.hTest(DenseMatrix(o.x)), o.features)).mkString("\n"))
  }
  test(withOffer)
  test(scores.take(10))
  test(scores.filter(_.contact.contains("olegych")))

  case class Feature[T](label: String, value: T) {
  }
  object Feature {
    implicit def toValue[T](f: Feature[T]): T = f.value
  }
  case class Score(values: Seq[String])(headers: Seq[String]) {
    lazy val valsByName = TreeMap(headers.zip(values): _*).filterNot(_._2.startsWith("="))
    lazy val valsByNameNotEmpty = valsByName.filter(_._2 != "empty")
    lazy val contact = computeSafely("Contact Info")(_.toLowerCase)(default = "")
    lazy val country = computeSafely("Country")(_.toLowerCase)(default = "")
    lazy val age = computeSafely("Age")(v => scale(BigDecimal(v), 0))
    lazy val gotOffer = computeSafely("Got the \"recruitment\" email that everybody is talking about? (Bold if Yes)")(_
      .toLowerCase.contains("yes"))(default = false)
    lazy val hws = (1 to 8).map(n => computeSafely("HW#n")(BigDecimal(_)))
    lazy val midterm = computeSafely("Midterm")(BigDecimal(_))
    lazy val finalGrade = computeSafely("Final")(BigDecimal(_))
    lazy val otherCourses = computeSafely("Attended other online Stanford courses (ML, DB...)?") {v =>
      Seq("ml", "db").map(c => c -> v.toLowerCase.contains(c)).toMap
    }(default = Map())
    lazy val score = Feature("Score", scale(midterm * 0.3 + finalGrade * 0.4 + average(hws.map(_.value).sorted
      .drop(2)) * 0.3))

    implicit val defaultBigDecimal = BigDecimal(0)
    def computeSafely[T](name: String)(transformer: String => T)(implicit default: T): Feature[T] = {
      Feature(name, try {transformer(valsByName(name))}
      catch {case e: Exception => {println("Got no #name in #valsByName"); default}})
    }

    def scale(b: BigDecimal, scale: Int = 3) = b.setScale(scale, RoundingMode.HALF_UP)
    def average(l: Seq[BigDecimal]) = l.sum / l.size
  }
  case class Offer(score: Score) {
    implicit def toDouble(b: BigDecimal) = b.toDouble
    implicit def toDoubleFeature(b: Feature[BigDecimal]) = Feature(b.label, b.value.toDouble)

    import score._

    val features =
      Seq[Feature[Double]](
        score.score
        , age
        //        , Feature("CountrySum", country.toCharArray.sum.toDouble)
        , midterm
        , finalGrade
        , Feature("Courses N", otherCourses.count(_._2 == true).toDouble)) ++
        score.hws.map(toDoubleFeature(_))
    val x = Array[Double](features.map(_.value): _*)
    val y: Double = if (score.gotOffer) 1.0 else 0.0
  }
}
