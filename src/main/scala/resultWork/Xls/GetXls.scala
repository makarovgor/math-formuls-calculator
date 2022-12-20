package resultWork.Xls

import org.apache.poi.ss.usermodel.WorkbookFactory

import java.io.File

object GetXls {

  def solve(n: Int) = {

    val wb = WorkbookFactory.create(new File("./src/main/resources/Build_approximation.xls"))
    var x = Seq.empty[Double]
    var y = Seq.empty[Double]

    (1 to n + 1).foreach { i =>
      val row = wb.getSheetAt(0).getRow(i)
      if (row != null) {
        x = x :+ row.getCell(0).toString.toDouble
        y = y :+ row.getCell(1).toString.toDouble
      }
    }
    (x, y)
  }

}
