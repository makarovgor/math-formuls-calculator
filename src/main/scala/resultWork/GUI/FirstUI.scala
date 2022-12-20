package resultWork.GUI

import Formuls.CalcSquare
import resultWork.Plot.Plot1
import resultWork.mainUI
import scalafx.Includes.jfxDialogPane2sfx
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control._
import scalafx.scene.layout.GridPane


object FirstUI {

  val SolveButton = new ButtonType("Solve", ButtonData.OKDone)
  val OkButton = new ButtonType("OK", ButtonData.OKDone)
  val BackButton = new ButtonType("Go back", ButtonData.Other)

  def start() = {

    case class Res(c: String)

    val menu_1Res = new Dialog[Res]() {
//          initOwner(stage)
      title = ""
      headerText = ""
    }

    def grid_1(str: String) = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)
      add(new Label(s"Area = $str"), 0, 1)
    }

    case class Equations(fstEq: String, sndEq: String, thrEq: String, eps: String)

    val menuFst = new Dialog[Equations]() {
      //    initOwner(stage)
      title = "Calculate the intersection area"
      headerText = "Please, enter equations"
    }

    menuFst.dialogPane().buttonTypes = Seq(SolveButton, ButtonType.Cancel)

    val firstEq = new TextField() {
      promptText = "x"
      text = "x"
    }
    val secondEq = new TextField() {
      promptText = "x"
      text = "x"
    }
    val thirdEq = new TextField() {
      promptText = "x"
      text = "x"
    }
    val epsilon = new TextField() {
      promptText = "0.1"
      text = "0.001"
    }

    val gridFst = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)

      add(new Label("First equation:"), 0, 0)
      add(firstEq, 1, 0)
      add(new Label("Second equation:"), 0, 1)
      add(secondEq, 1, 1)
      add(new Label("Third equation:"), 0, 2)
      add(thirdEq, 1, 2)
      add(new Label("Epsilon"), 0, 4)
      add(epsilon, 1, 4)
    }

    menuFst.dialogPane().content = gridFst

    solve(menuFst)

    def solve(pp: Dialog[Equations]): Unit = {
      pp.resultConverter = dialogButton =>
        if (dialogButton == SolveButton)
          Equations(firstEq.text(), secondEq.text(), thirdEq.text(), epsilon.text())
        else
          null

      menuFst.showAndWait() match {
        case Some(Equations(f, s, t, eps)) =>
          val plot = new Plot1()
          val res = CalcSquare.solve(f, s, t, eps.toDouble)
          menu_1Res.dialogPane().content = grid_1(f"${res._1}%1.14f")
          plot.draw(res._2, res._3, f, s ,t)
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