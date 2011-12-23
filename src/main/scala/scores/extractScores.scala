package scores

import org.springframework.core.io.ClassPathResource
import org.apache.poi.hssf.usermodel.{HSSFSheet, HSSFWorkbook}
import org.apache.poi.ss.util.CellUtil
import scala.collection.immutable.IndexedSeq
import org.apache.poi.ss.usermodel.Cell

class extractScores(val filename: String = "AI class scores.xls") {
  private def readFile(filename: String): HSSFWorkbook = {
    new HSSFWorkbook(new ClassPathResource(filename).getInputStream)
  }
  val wb = readFile(filename)
  def getAllScoreRows: IndexedSeq[IndexedSeq[String]] = {
    (for (k <- 0 until wb.getNumberOfSheets; if wb.getSheetName(k) == "Scores") yield getScoreRows(k)).flatten
  }
  def getScoreRows(k: Int): IndexedSeq[IndexedSeq[String]] = {
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
    allRows
  }

}