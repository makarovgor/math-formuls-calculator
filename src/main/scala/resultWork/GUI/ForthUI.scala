package resultWork.GUI

import Formuls.{FredholmFirst, FredholmSecond}
import javafx.event.ActionEvent
import resultWork.GUI.FirstUI._
import resultWork.Plot.Plot4
import resultWork.Xls.FillXls
import resultWork.mainUI
import scalafx.Includes.jfxDialogPane2sfx
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.GridPane

object ForthUI {

  def start() = {

    case class Res(c: String)

    val menu_1Res = new Dialog[Res]() {
      title = ""
      headerText = ""
    }

    def grid(str: String, a: String, b: String) = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)
      add(new Label(s"Result of calculating was saved to Fredholm_$str.xls"), 0, 1)
      add(new Label(s"Linear regression"), 0, 3)
      add(new Label(s"y = $a * x + $b"), 0, 4)
    }

    case class Equations(core: String, func: String, sndEq: String, thrEq: String, eps: String)

    val menuFst = new Dialog[Equations]() {
      title = "Solve integral equation"
      headerText = "Fredholm second kind"
    }

    menuFst.dialogPane().buttonTypes = Seq(SolveButton, ButtonType.Cancel)

    ///////

    val core = new TextField() {
      text = "x"
    }
    val func = new TextField() {
      text = "x"
    }
    val from = new TextField() {
      text = "-1"
    }
    val to = new TextField() {
      text = "1"
    }
    val partitions = new TextField() {
      text = "4"
    }

    val gridFst = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)

      add(new Label("Core of equation:  R(x, k)"), 0, 0)
      add(core, 1, 0)
      add(new Label("Function:"), 0, 1)
      add(func, 1, 1)
      val labelFrom = new Label("From:")
      add(labelFrom, 0, 3)
      add(from, 0, 4)
      val labelTo = new Label("To:")
      add(labelTo, 1, 3)
      add(to, 1, 4)
      add(new Label("Number of partitions"), 0, 6)
      add(partitions, 0, 7)

      val buttonTwo = new Button("Fredholm second kind")
      add(buttonTwo, 3, 0)

      val buttonOne = new Button("Fredholm first kind")
      add(buttonOne, 3, 1)

      buttonTwo.onAction = (e: ActionEvent) => {
        menuFst.headerText = "Fredholm second kind"
      }

      buttonOne.onAction = (e: ActionEvent) => {
        menuFst.headerText = "Fredholm first kind"
      }
    }

    menuFst.dialogPane().content = gridFst

    solve(menuFst)

    def solve(pp: Dialog[Equations]): Unit = {
      pp.resultConverter = dialogButton =>
        if (dialogButton == SolveButton)
          Equations(core.text(), func.text(), from.text(), to.text(), partitions.text())
        else
          null

      menuFst.showAndWait() match {
        case Some(Equations(cor, fun, min, max, n)) =>
          val plot = new Plot4()
          if (menuFst.headerText.value.contains("second")) {
            val (res, a, b) = FredholmSecond.solve(cor, fun, min.toDouble, max.toDouble, n.toInt)
            menu_1Res.dialogPane().content = grid("second", a.toString, b.toString)
            FillXls.solve(res, fun, "second", a, b)
            plot.draw(res, fun)
          } else {
            val (res, a, b) = FredholmFirst.solve(cor, fun, min.toDouble, max.toDouble, n.toInt)
            menu_1Res.dialogPane().content = grid("first", a.toString, b.toString)
            FillXls.solve(res, fun, "first", a, b)
            plot.draw(res, fun)
          }
          menu_1Res.dialogPane().buttonTypes = Seq(OkButton, BackButton)
          menu_1Res.showAndWait() match {
            case Some(OkButton) =>
              mainUI.start()
              menu_1Res.close()
            case Some(BackButton) =>
              mainUI.start()
              solve(menuFst)
          }

        case None =>
          menuFst.hide()
      }
    }
  }

}
