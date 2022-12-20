package resultWork.Plot

import resultWork.mainUI.stage
import resultWork.parser.facaiy.MathExp
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}

import scala.math.abs

class Plot2 {

  def draw(f: String, a: Int, b: Int, min: Double) = {

    val xAxis = new NumberAxis()
    //    xAxis.label = "Number of Month"
    val yAxis = new NumberAxis()

    val lineChart = LineChart(xAxis, yAxis)

    lineChart.title = "Function"

    val x = a to b by (abs(b - a) / 30)

    val y0 = XYChart.Data[Number, Number](x.head, MathExp.parse(f).eval(Map("x" -> x.head)))

    val res = (1 until x.size).map { i =>
      XYChart.Data[Number, Number](x(i), MathExp.parse(f).eval(Map("x" -> x(i))))
    }

    val data = ObservableBuffer(y0).addAll(res)

    val series = XYChart.Series[Number, Number]("Y", data)

    lineChart.getData.add(series)

    val scattY = XYChart.Data[Number, Number](min, MathExp.parse(f).eval(Map("x" -> min)))
    val scattSer = XYChart.Series[Number, Number]("", ObservableBuffer(scattY))
    lineChart.getData.add(scattSer)

    val scene = new Scene(800, 600) {
      root = lineChart
    }
    stage.setScene(scene)
    stage.show()
  }
}
