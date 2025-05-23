import GameEngine.gameStateAtual
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{AnchorPane, GridPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.text.TextFlow
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.scene.media.{Media, MediaPlayer}
import java.io.File


class SceneController {
  @FXML
  private var label1: VBox = _
  @FXML
  private var gameTitle: Label = _
  @FXML
  private var startButton: Button = _
  @FXML
  private var exitButton1: Button = _
  @FXML
  private var option: Button = _





  // Main Menu!
  def changeColor() = {
    if(gameTitle.getTextFill.equals(Color.BLACK)){
      gameTitle.setTextFill(Color.WHITE)
    } else if(gameTitle.getTextFill.equals(Color.WHITE)){
      gameTitle.setTextFill(Color.BLACK)
    }
  }

  def switchToGame() = {
    val loader = new FXMLLoader(getClass.getResource("gameBoard.fxml"))
    val root: Parent = loader.load()
    val stage = startButton.getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))

    //switch to game
    GameEngine.startGame()
  }

  def exitMenu(): Unit = {
    val stage = label1.getScene.getWindow
    stage.hide()
    println("Shutting down...")
  }

  def switchToOptions() = {
    val loader = new FXMLLoader(getClass.getResource("options.fxml"))
    val root: Parent = loader.load()
    val stage = option.getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))

    //switch to game
    //GameEngine.options()
  }






  @FXML
  private var state: TextFlow = _
  @FXML
  private var label2: AnchorPane = _
  @FXML
  private var randomMove: Button = _
  @FXML
  private var board: GridPane = _
  @FXML
  private var exitButton2: Button = _

  // gameBoard!
  def randomPlay(): Unit = {
    //GameEngine.playRandomly()
    //println("Jogada aleatória!") TEXTFLOW
  }

  def play(): Unit = {
    //    val event = javafx.scene.input.MouseEvent
    //    val clickedNode = event.getPickResult.getIntersectedNode
    //    if(clickedNode != board) {
    //      val colIndex = GridPane.getColumnIndex(clickedNode)
    //      val rowIndex = GridPane.getRowIndex(clickedNode)
    //      println("Mouse clicked on: " + colIndex + ", " + rowIndex)
    val circle = new Circle()
    circle.setRadius(30)
    circle.setFill(Color.BLACK)
    board.add(circle,1,3)
  }

  //val play = (x,y)
  //val gameState = GameEngine.
  //GameEngine.play(play, )

  def exitGame(): Unit = {
    val stage = label2.getScene.getWindow
    stage.hide()
    println("Shutting down...")
  }

  @FXML
  def undoJogada(): Unit = {
    GameEngine.undo()
    println("Jogada desfeita.")
    // Se necessário, atualiza o tabuleiro no UI aqui
  }
}