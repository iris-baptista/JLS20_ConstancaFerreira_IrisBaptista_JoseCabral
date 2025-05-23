import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{AnchorPane, GridPane}
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage



class SceneController {
  val gameState = {
    val (cena,outracena) = GameEngine.newBoard(5, Nil,Nil,5)
    GameEngine.novaJogada(5,0,0,cena,outracena)}

  @FXML
  private var label1: AnchorPane = _
  @FXML
  private var gameTitle: Label = _
  @FXML
  private var startButton: Button = _
  @FXML
  private var exitButton1: Button = _

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
    GameEngine.printBoard(gameState.board)
    println()
    GameEngine.turno(gameState)
  }

  def exitMenu(): Unit = {
    val stage = label1.getScene.getWindow
    stage.hide()
    println("Shutting down...")
  }

  @FXML
  private var s00: Circle = _
  @FXML
  private var s01: Circle = _
  @FXML
  private var s02: Circle = _
  @FXML
  private var s03: Circle = _
  @FXML
  private var s04: Circle = _
  @FXML
  private var s10: Circle = _
  @FXML
  private var s11: Circle = _
  @FXML
  private var s12: Circle = _
  @FXML
  private var s13: Circle = _
  @FXML
  private var s14: Circle = _
  @FXML
  private var s20: Circle = _
  @FXML
  private var s21: Circle = _
  @FXML
  private var s22: Circle = _
  @FXML
  private var s23: Circle = _
  @FXML
  private var s24: Circle = _
  @FXML
  private var s30: Circle = _
  @FXML
  private var s31: Circle = _
  @FXML
  private var s32: Circle = _
  @FXML
  private var s33: Circle = _
  @FXML
  private var s34: Circle = _
  @FXML
  private var s40: Circle = _
  @FXML
  private var s41: Circle = _
  @FXML
  private var s42: Circle = _
  @FXML
  private var s43: Circle = _
  @FXML
  private var s44: Circle = _

  //@FXML
  //private var state: TextFlow = _
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
    GameEngine.playRandomly()
    //println("Jogada aleatória!") TEXTFLOW
  }

  def play(x: Int, y:Int): Unit = {
    //val play = (x,y)
    //val gameState = GameEngine.
    //GameEngine.play(play, )
  }

  def exitGame(): Unit = {
    val stage = label2.getScene.getWindow
    stage.hide()
    println("Shutting down...")
  }
}
