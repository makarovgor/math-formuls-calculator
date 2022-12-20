package resultWork.Plot

import resultWork.mainUI.stage
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}

class Plot3 {

  def draw(x: Seq[Double], y: Seq[Double], a: Double, b: Double) = {

    val xAxis = new NumberAxis()
    //    xAxis.label = "Number of Month"
    val yAxis = new NumberAxis()

    val lineChart = LineChart(xAxis, yAxis)
    lineChart.title = "Function"

    val regress = x.map(i => i * a + b)

    val y0 = XYChart.Data[Number, Number](x.head, regress.head)

    val res = (1 until  x.size).map { i =>
      XYChart.Data[Number, Number](x(i), regress(i))
    }

    val data = ObservableBuffer(y0).addAll(res)

    val series = XYChart.Series[Number, Number]("Y", data)

    x.indices.foreach{ i =>
      val scattY =  XYChart.Data[Number, Number](x(i), y(i))
      val scattSer = XYChart.Series[Number, Number]("", ObservableBuffer(scattY))
      lineChart.getData.add(scattSer)
    }

    lineChart.getData.add(series)

    val scene = new Scene(800, 600) {
      root = lineChart
//      root = scatterChart
    }
    stage.setScene(scene)
    stage.show()
  }

}
