package resultWork.Plot

import resultWork.mainUI.stage
import resultWork.parser.facaiy.MathExp
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}

class Plot4 {

  def draw(x: Seq[Double], f: String) = {

    val xAxis = new NumberAxis()
//    xAxis.label = "Number of Month"
    val yAxis = new NumberAxis()

    val lineChart = LineChart(xAxis, yAxis)

    lineChart.title = "Function"

    val y0 = XYChart.Data[Number, Number](x.head, MathExp.parse(f).eval(Map("x" -> x.head)))

    val res = (1 until  x.size).map { i =>
      XYChart.Data[Number, Number](x(i), MathExp.parse(f).eval(Map("x" -> x(i))))
    }

    val data = ObservableBuffer(y0).addAll(res)


    val series = XYChart.Series[Number, Number]("Y", data)


    lineChart.getData.add(series)

    val scene = new Scene(800, 600) {
      root = lineChart
    }
    stage.setScene(scene)
    stage.show()

//    stage = new PrimaryStage {
//      title = "Line Chart Sample"
//      scene = new Scene(800, 600) {
//        root = lineChart
//      }
//    }
  }

}