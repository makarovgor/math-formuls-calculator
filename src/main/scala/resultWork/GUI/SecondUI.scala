package resultWork.GUI

import Formuls.{GoldenRatio, Newton}
import javafx.event.ActionEvent
import resultWork.GUI.FirstUI._
import resultWork.Plot.Plot2
import resultWork.mainUI
import scalafx.Includes.jfxDialogPane2sfx
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.GridPane

object SecondUI {

  def start() = {

    case class Res(c: String)

    val menu_1Res = new Dialog[Res]() {
      //          initOwner(stage)
      title = ""
      headerText = ""
    }

    def grid_1(x: String, y: String) = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)
      add(new Label(s"min X value = $x"), 0, 1)
      add(new Label(s"function value = $y"), 0, 2)
    }

    def grid_2(x: String, y: String) = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)
      add(new Label(s"min X value = $x"), 0, 1)
      add(new Label(s"min Y value = $y"), 0, 2)
    }


    case class Equations(fstEq: String, sndEq: String, thrEq: String, eps: String)

    val menuFst = new Dialog[Equations]() {
      //    initOwner(stage)
      title = "Find minimum of a function"
      headerText = "Please, enter equation and params"
    }

    menuFst.dialogPane().buttonTypes = Seq(SolveButton, ButtonType.Cancel)

    ///////

    val firstEq = new TextField() {
      promptText = "x"
      text = "x"
    }
    val from = new TextField() {
      promptText = "x"
      text = "-100"
    }
    val to = new TextField() {
      promptText = "x"
      text = "100"
    }
    val epsilon = new TextField() {
      promptText = "0.1"
      text = "0.001"
    }

    val gridFst = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)

      add(new Label("Equation:"), 0, 0)
      add(firstEq, 0, 1)
      val labelFrom = new Label("From:")
      add(labelFrom, 0, 2)
      add(from, 0, 3)
      val labelTo = new Label("To:")
      add(labelTo, 1, 2)
      add(to, 1, 3)
      add(new Label("Epsilon"), 0, 5)
      add(epsilon, 0, 6)

      val buttonTwo = new Button("local minimum with two params")
      add(buttonTwo, 3, 0)

      val buttonOne = new Button("extremum with one param")
      add(buttonOne, 3, 1)

      buttonTwo.onAction = (e: ActionEvent) => {
        labelFrom.text = "X value"
        labelTo.text = "Y value"
        firstEq.text = "x + y"
      }

      buttonOne.onAction = (e: ActionEvent) => {
        labelFrom.text = "From:"
        labelTo.text = "To:"
        firstEq.text = "x"
      }
    }

    menuFst.dialogPane().content = gridFst

    solve(menuFst)

    def solve(pp: Dialog[Equations]): Unit = {
      pp.resultConverter = dialogButton =>
        if (dialogButton == SolveButton)
          Equations(firstEq.text(), from.text(), to.text(), epsilon.text())
        else
          null

      menuFst.showAndWait() match {
        case Some(Equations(f, s, t, eps)) =>
          var res = (0.0, 0.0)
          val plot = new Plot2()
          if (gridFst.labelFrom.text.delegate.getValue == "From:") {
            res = GoldenRatio.find(f, s.toDouble, t.toDouble, eps.toDouble)
            menu_1Res.dialogPane().content = grid_1(f"${res._1}%1.14f", f"${res._2}%1.14f")
            plot.draw(f, s.toInt, t.toInt, res._1)
          } else {
            res = Newton.solve(f, s.toDouble, t.toDouble, eps.toDouble)
            menu_1Res.dialogPane().content = grid_2(f"${res._1}%1.14f", f"${res._2}%1.14f")
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

        case None => menuFst.close()
      }
    }
  }

}
