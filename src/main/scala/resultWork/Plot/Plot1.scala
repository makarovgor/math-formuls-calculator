package resultWork.Plot

import resultWork.mainUI.stage
import resultWork.parser.facaiy.MathExp
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}

import scala.math.abs

class Plot1 {

  def draw(x1: Double, x2: Double, f1: String, f2: String, f3: String) = {
    val dist = abs(x2 - x1)

    def buildLine(f: String, i: Int, xx: Seq[Double]) = {
      val y0 = XYChart.Data[Number, Number](xx.head, MathExp.parse(f).eval(Map("x" -> xx.head)))

      val res = (1 until xx.size).map { i =>
        XYChart.Data[Number, Number](xx(i), MathExp.parse(f).eval(Map("x" -> xx(i))))
      }

      val data = ObservableBuffer(y0).addAll(res)

      XYChart.Series[Number, Number](s"F$i", data)
    }

    val xAxis = new NumberAxis()
    val yAxis = new NumberAxis()

    val lineChart = LineChart(xAxis, yAxis)
    lineChart.title = "Function"

    if (dist == 0.0) {
      val steps = (-10 to 10).map(_.toDouble)
      lineChart.getData.add(buildLine(f1, 1, steps))
    } else {

      def evalSteps() = {
        import scala.math.BigDecimal.double2bigDecimal
        (x1 - dist to (x2 + dist) by (dist / 7)).map(_.toDouble)
      }

      val steps = evalSteps()

      lineChart.getData.add(buildLine(f1, 1, steps))
      lineChart.getData.add(buildLine(f2, 2, steps))
      lineChart.getData.add(buildLine(f3, 3, steps))
    }

    val scene = new Scene(800, 600) {
      root = lineChart
    }
    stage.setScene(scene)
    stage.show()
  }

}
