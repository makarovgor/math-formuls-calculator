package resultWork.GUI

import Formuls.FredholmSecond.findApprox
import resultWork.GUI.FirstUI._
import resultWork.Plot.Plot3
import resultWork.Xls.GetXls
import resultWork.mainUI
import scalafx.Includes.jfxDialogPane2sfx
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout.GridPane

object ThirdUI {
  def start() = {

    case class Res(c: String)

    val menu_1Res = new Dialog[Res]() {
      title = ""
      headerText = ""
    }

    def grid_1(a: Double, b: Double) = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)
      add(new Label("Approximation graph was built in Build_approximation.xls"), 0, 1)
      add(new Label(s"Linear regression"), 0, 1)
      add(new Label(s"y = $a * x + $b"), 0, 4)
    }

    val menuFst = new Dialog[Res]() {
      title = "Build approximation"
    }

    menuFst.dialogPane().buttonTypes = Seq(SolveButton, ButtonType.Cancel)

    val nums = new TextField() {
      text = "10"
    }

    val gridFst = new GridPane() {
      hgap = 20
      vgap = 20
      padding = Insets(20, 100, 10, 10)

      add(new Label("Please, enter X and Y values at Build_approximation.xls to build approximate function"), 0, 0)
      add(new Label("Count of raws"), 0, 2)
      add(nums, 0, 3)
      add(new Label("""Then press "Solve" button"""), 0, 5)
    }

    menuFst.dialogPane().content = gridFst

    solve(menuFst)

    def solve(pp: Dialog[Res]): Unit = {
      pp.resultConverter = dialogButton =>
        if (dialogButton == SolveButton)
          Res(nums.text())
        else
          null

      menuFst.showAndWait() match {
        case Some(Res(num)) =>
          val plot = new Plot3()
          val (x, y) = GetXls.solve(num.toInt)
          val (a, b) = findApprox(x, y)
          menu_1Res.dialogPane().content = grid_1(a, b)
          plot.draw(x, y, a, b)
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
