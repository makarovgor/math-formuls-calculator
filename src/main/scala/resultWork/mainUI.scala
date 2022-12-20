package resultWork

import javafx.event.ActionEvent
import resultWork.GUI.{FirstUI, ForthUI, SecondUI, ThirdUI}
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label}

object mainUI extends JFXApp {

  def start(): Unit = {

    stage = new JFXApp.PrimaryStage {
      title = "Main Menu"
      scene = new Scene(300, 200) {
        val label = new Label("")
        label.layoutX = 20
        label.layoutY = 20

        val button1 = new Button("Calculate the intersection area")
        button1.layoutX = 20
        button1.layoutY = 20

        val button2 = new Button("Find minimum of a function")
        button2.layoutX = 20
        button2.layoutY = 60

        val button3 = new Button("Build approximation")
        button3.layoutX = 20
        button3.layoutY = 100

        val button4 = new Button("Solve integral equation")
        button4.layoutX = 20
        button4.layoutY = 140

        content = List(label, button1, button2, button3, button4)
        button1.onAction = (e: ActionEvent) => {
          FirstUI.start()
        }

        button2.onAction = (e: ActionEvent) => {
          SecondUI.start()
        }

        button3.onAction = (e: ActionEvent) => {
          ThirdUI.start()
        }

        button4.onAction = (e: ActionEvent) => {
          ForthUI.start()
        }
      }
    }
  }

    start()

}
