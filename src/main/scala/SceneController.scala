import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Button, Label}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{AnchorPane, StackPane}
//import javafx.scene.shape.{Rectangle}
import javafx.fxml.FXMLLoader
import javafx.scene.layout.GridPane
import javafx.scene.paint.Color
import javafx.stage.Stage



class SceneController {

  @FXML
  private var startButton: Button = _
  @FXML
  private var exitButton1: Button = _
  @FXML
  private var exitButton2: Button = _
  @FXML
  private var gameTitle: Label = _
  @FXML
  private var randomMove: Button = _
  //private var state: TextFlow = _
  @FXML
  private var board: GridPane = _

  @FXML
  private var label1: AnchorPane = _

  @FXML
  private var label2: AnchorPane = _


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
  }

  def exitMenu(): Unit = {
    val stage = label1.getScene.getWindow
    stage.hide()
    println("Shutting down...")
  }

//  @FXML
//  private var rect00: Rectangle = _
//  private var rect01: Rectangle = _
//  private var rect02: Rectangle = _
//  private var rect03: Rectangle = _
//  private var rect04: Rectangle = _
//  private var rect10: Rectangle = _
//  private var rect11: Rectangle = _
//  private var rect12: Rectangle = _
//  private var rect13: Rectangle = _
//  private var rect14: Rectangle = _
//  private var rect20: Rectangle = _

  // gameBoard!
  def randomPlay(): Unit = {
    //GameEngine.playRandomly()
    //println("Jogada aleatória!") TEXTFLOW
  }

  def exitGame(): Unit = {
    val stage = label2.getScene.getWindow
    stage.hide()
    println("Shutting down...")
  }

  def showCredits(): Unit = {
    // Carrega a imagem a partir dos resources
    val imageUrl = getClass.getResource("/credits.png")
    if (imageUrl == null) {
      println("Imagem credits.png não encontrada!")
      return
    }

    val image = new Image(imageUrl.toExternalForm)
    val imageView = new ImageView(image)

    // Ajusta o tamanho se necessário
    imageView.setPreserveRatio(true)
    imageView.setFitWidth(500)  // podes ajustar isto

    val root = new StackPane(imageView)
    val scene = new Scene(root, 600, 400)  // tamanho da janela

    val stage = new Stage()
    stage.setTitle("Créditos")
    stage.setScene(scene)
    stage.show()
  }
}
