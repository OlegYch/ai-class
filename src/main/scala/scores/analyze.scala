package scores

import org.apache.poi.ss.usermodel.Cell
import org.springframework.core.io.ClassPathResource
import scala.collection.immutable.TreeMap
import org.apache.poi.hssf.usermodel.{HSSFSheet, HSSFWorkbook}
import org.apache.poi.ss.util.CellUtil
import scala.math.BigDecimal.RoundingMode
import scalala.tensor.dense.DenseMatrix

@EnhanceStrings
object analyze extends App {
  private def readFile(filename: String): HSSFWorkbook = {
    new HSSFWorkbook(new ClassPathResource(filename).getInputStream)
  }
  var wb = readFile("AI class scores.xls")
  System.out.println("Data dump:\n")
  for (k <- 0 until wb.getNumberOfSheets; if wb.getSheetName(k) == "Scores") {
    val sheet: HSSFSheet = wb.getSheetAt(k)
    val numberOfRows: Int = sheet.getPhysicalNumberOfRows
    System.out.println("Sheet " + k + " \"" + wb.getSheetName(k) + "\" has " + numberOfRows + " row(s).")
    val rows = for {
      r <- 0 until numberOfRows
      row <- Option(sheet.getRow(r))
    } yield row
    val cellsNumber = rows.map(_.getPhysicalNumberOfCells).max
    val allRows = for (row <- rows) yield {
      //      System.out.println("\nROW " + row.getRowNum + " has " + cellsNumber + " cell(s).")
      for {
        c <- 0 until cellsNumber
        cell = CellUtil.getCell(row, c)
      } yield {
        val value = cell.getCellType match {
          //          case Cell.CELL_TYPE_FORMULA =>
          //            "FORMULA value=" + cell.getCellFormula
          case Cell.CELL_TYPE_NUMERIC =>
            "" + cell.getNumericCellValue
          case Cell.CELL_TYPE_STRING =>
            "" + cell.getStringCellValue
          case _ => "empty"
        }
        //        System.out.println("CELL col=" + cell.getColumnIndex + " VALUE=" + value)
        value
      }
    }
    val scores = allRows.filter(_.exists(_ != "empty")).map(vals => Score(vals)(allRows(0)))
    println(scores.map(_.valsByName))
    val withOffer = scores.filter(_.gotOffer).sortBy(-_.score)
    println(TreeMap(withOffer.groupBy(_.age.value).toSeq: _*).map {
      case (_, scores) => scores.map(s => (s.score.value, s.valsByNameNotEmpty)).mkString("\n")
    }.mkString("\n"))
    println(withOffer.size)
    val ml = new ml(scores)
    def test(scores: IndexedSeq[Score]) {
      println(scores.map(o => (ml.hTest(DenseMatrix(Offer(o).x)), Offer(o).features)).mkString("\n"))
    }
    test(withOffer)
    test(scores.take(10))
    test(scores.filter(_.contact.contains("olegych")))
  }

  case class Feature[T](label: String, value: T){
  }
  object Feature {
    implicit def toValue[T](f:Feature[T]):T = f.value
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
    lazy val score = Feature("Score", scale(midterm * 0.3 + finalGrade * 0.4 + average(hws.map(_.value).sorted.drop(2)) * 0.3))

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
        , Feature("Courses N", otherCourses.count(_._2 == true).toDouble))++
        score.hws.map(toDoubleFeature(_))
    val x = Array[Double](features.map(_.value): _*)
    val y: Double = if (score.gotOffer) 1.0 else 0.0
  }
}
