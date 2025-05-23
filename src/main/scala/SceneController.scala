import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.AnchorPane
import javafx.fxml.FXMLLoader
import javafx.scene.layout.GridPane
import javafx.scene.paint.Color
import javafx.stage.Stage
import java.net.URL

class SceneController {

  @FXML
  private var startButton: Button = _
  @FXML
  private var gameTitle: Label = _
  @FXML
  private var label1: AnchorPane = _

  // Adicione este método de inicialização
  @FXML
  def initialize(): Unit = {
    println("Initializing controller...")
    loadBackground()
  }

  private def loadBackground(): Unit = {
    try {
      // Verifique o caminho do recurso
      val imageUrl: URL = getClass.getResource("/background.jpg")
      if (imageUrl == null) {
        println("ERRO: Não encontrou a imagem background.jpg no classpath!")
        // Listar conteúdo do diretório para debug
        val dir = getClass.getResource("/")
        println(s"Conteúdo do diretório raiz de recursos: $dir")
      } else {
        println(s"Imagem encontrada em: ${imageUrl.toExternalForm()}")
        label1.setStyle(
          s"-fx-background-image: url('${imageUrl.toExternalForm()}'); " +
            "-fx-background-size: cover; -fx-background-position: center; " +
            "-fx-background-repeat: no-repeat;"
        )
      }
    } catch {
      case e: Exception =>
        println(s"Erro ao carregar background: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  // Restante do seu código permanece igual...
  def changeColor(): Unit = {
    if(gameTitle.getTextFill.equals(Color.BLACK)) {
      gameTitle.setTextFill(Color.WHITE)
    } else if(gameTitle.getTextFill.equals(Color.WHITE)) {
      gameTitle.setTextFill(Color.BLACK)
    }
  }

  def switchToGame(): Unit = {
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
}