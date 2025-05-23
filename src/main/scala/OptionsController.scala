import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{StackPane, VBox}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.TextField
import javafx.stage.Stage

import java.awt.event.ActionEvent

class OptionsController {

  @FXML private var bg: VBox = _

  @FXML
  def showCredits(): Unit = {
    // Tenta obter o Stage a partir do VBox bg
    val stage = bg.getScene.getWindow.asInstanceOf[Stage]

    val image = new Image(getClass.getResource("creditos.png").toExternalForm)
    val imageView = new ImageView(image)
    imageView.setPreserveRatio(true)
    imageView.setFitWidth(600)

    val root = new StackPane(imageView)
    val scene = new Scene(root)

    stage.setScene(scene)
  }

  @FXML
  var tempoTextField: TextField = _ // campo de texto para o usuário digitar o tempo

  @FXML
  var capturasTextField: TextField = _


  @FXML
  def mudarTempoDeJogada(): Unit = {
    try {
      val novoTempo = tempoTextField.getText.toInt
      GameEngine.tempoDeJogada = novoTempo
      println(s"Tempo de jogada alterado para $novoTempo segundos")
    } catch {
      case _: NumberFormatException =>
        println("Valor inválido para tempo de jogada")
    }
  }


  @FXML
  def mudarNumeroDeCapturas(): Unit = {
    try {
      val novoNumero = capturasTextField.getText.toInt
      GameEngine.numeroDeCapturas = novoNumero
      println(s"Número de capturas alterado para $novoNumero")
    } catch {
      case _: NumberFormatException =>
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
    } catch {
      case e: Exception =>
        e.printStackTrace()
        println("Erro ao carregar o menu principal.")
    }
  }

}