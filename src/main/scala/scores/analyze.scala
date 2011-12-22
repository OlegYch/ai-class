package scores

import org.apache.poi.ss.usermodel.Cell
import org.springframework.core.io.ClassPathResource
import scala.collection.immutable.TreeMap
import org.apache.poi.hssf.usermodel.{HSSFSheet, HSSFWorkbook}
import org.apache.poi.ss.util.CellUtil
import scala.math.BigDecimal.RoundingMode

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
    println(TreeMap(withOffer.groupBy(_.age).toSeq: _*).map {
      case (_, scores) => scores.map(s => (s.score, s.valsByNameNotEmpty)).mkString("\n")
    }.mkString("\n"))
    println(withOffer.size)
  }

  case class Score(values: Seq[String])(headers: Seq[String]) {
    lazy val valsByName = TreeMap(headers.zip(values): _*).filterNot(_._2.startsWith("="))
    lazy val valsByNameNotEmpty = valsByName.filter(_._2 != "empty")
    lazy val age = scale(computeSafely(BigDecimal(valsByName("Age"))), scale = 0)
    lazy val gotOffer = computeSafely(valsByName("Got the \"recruitment\" email that everybody is talking about? " +
      "(Bold if Yes)")
      .toLowerCase.contains("yes"))(default = false)
    lazy val score = scale(computeSafely(BigDecimal(valsByName("Midterm")) * 0.3 +
      BigDecimal(valsByName("Final")) * 0.4 +
      average(valsByName.from("HW1").to("HW8").values.map(BigDecimal(_)).toSeq.sorted.drop(2)) * 0.3))

    implicit val defaultBigDecimal = BigDecimal(0)
    def computeSafely[T](decimal1: => T)(implicit default: T): T = {
      try {decimal1} catch {case e: Exception => {println("Exception #valsByName"); default}}
    }

    def scale(b: BigDecimal, scale: Int = 3) = b.setScale(scale, RoundingMode.HALF_UP)
    def average(l: Seq[BigDecimal]) = l.sum / l.size
  }
}
