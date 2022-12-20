package resultWork.Xls

import org.apache.poi.hssf.usermodel.HSSFWorkbook
import resultWork.parser.facaiy.MathExp

import java.io._

object FillXls {

  def solve(res: Seq[Double], f: String, str: String, a: Double, b: Double) = {

    val workbook = new HSSFWorkbook();
    val sheet = workbook.createSheet("First")

    var rowNum = 0
    var row = sheet createRow rowNum
    row.createCell(0).setCellValue("X")
    row.createCell(1).setCellValue("F")
    row.createCell(2).setCellValue("a")
    row.createCell(3).setCellValue("b")

    rowNum = 1
    row = sheet createRow rowNum
    row.createCell(0).setCellValue(res.head)
    row.createCell(1).setCellValue(MathExp.parse(f).eval(Map("x" -> res.head)))
    row.createCell(2).setCellValue(a)
    row.createCell(3).setCellValue(b)

    (2 to res.size).foreach { i =>
      rowNum = i
      row = sheet createRow rowNum
      row.createCell(0).setCellValue(res(i - 1))
      row.createCell(1).setCellValue(MathExp.parse(f).eval(Map("x" -> res(i - 1))))
    }

    val out = new FileOutputStream(new File(s"./src/main/resources/Fredholm_$str.xls"))
    workbook.write(out)
    out.close()
  }
}
