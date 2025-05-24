import javafx.fxml.{FXML, FXMLLoader}
import javafx.geometry.Pos
import javafx.scene.control.{Button, TextField}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{StackPane, VBox}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class OptionsController {
  @FXML private var bg: VBox = _

  @FXML
  def showCredits(): Unit = {
    // Tenta obter o Stage a partir da VBox "bg"
    val stage = bg.getScene.getWindow.asInstanceOf[Stage]

    val image = new Image(getClass.getResource("creditos.png").toExternalForm)
    val imageView = new ImageView(image)
    imageView.setPreserveRatio(true)
    imageView.setFitWidth(600)

    //criar button para sair dos creditos
    val backButton = new Button("Back")
    backButton.setOnAction(event => {
      val state= (event.getSource.asInstanceOf[javafx.scene.Node]).getScene.getWindow.asInstanceOf[Stage]
      voltarMenuCreditos(stage)})
    backButton.setText("Back to Menu")

    //VBox deixa por o botao em cima da img
    val buttonContainer = new VBox(backButton)
    buttonContainer.setAlignment(Pos.BOTTOM_CENTER) // Change to CENTER or BOTTOM_RIGHT if desired
    buttonContainer.setPickOnBounds(false) // Don't block image interaction

    val root = new StackPane(imageView, backButton)
    val scene = new Scene(root)

    stage.setScene(scene)
  }

  @FXML
  var tempoTextField: TextField = _ //campo de texto para o usuário indicar o tempo
  @FXML
  var capturasTextField: TextField = _ //campo de texto para o numero de pecas a capturar

  @FXML
  def mudarTempoDeJogada(): Unit = {
    try {
      val novoTempo = tempoTextField.getText.toInt
      GameEngine.tempoDeJogada = novoTempo
      println(s"Tempo de jogada alterado para $novoTempo segundos")
    }
    catch {
      case _: NumberFormatException => //se nao passou um numero inteiro
        println("Valor inválido para tempo de jogada")
    }
  }

  @FXML
  def mudarNumeroDeCapturas(): Unit = {
    try {
      val novoNumero = capturasTextField.getText.toInt
      GameEngine.numeroDeCapturas = novoNumero
      println(s"Número de capturas alterado para $novoNumero")
    }
    catch {
      case _: NumberFormatException => //se nao passou um numero inteiro
        println("Valor inválido para número de capturas")
    }
  }

  @FXML
  def voltarMenu(): Unit = {
    val stage = bg.getScene.getWindow.asInstanceOf[Stage]
    try {
      val loader = new FXMLLoader(getClass.getResource("mainMenu.fxml"))
      val root: Parent = loader.load()
      val scene = new Scene(root)
      stage.setScene(scene)
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        println("Erro ao carregar o menu principal.")
    }
  }

  @FXML //quando queremos voltar a partir dos creditos
  def voltarMenuCreditos(stage: Stage): Unit = {
    try {
      val loader = new FXMLLoader(getClass.getResource("mainMenu.fxml"))
      val root: Parent = loader.load()
      val scene = new Scene(root)
      stage.setScene(scene)
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        println("Erro ao carregar o menu principal.")
    }
  }
}
