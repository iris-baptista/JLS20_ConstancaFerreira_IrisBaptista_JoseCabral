import GameEngine.gameStateAtual
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label}
import GameEngine.Coord2D
import javafx.application.Platform
import javafx.scene.layout.{AnchorPane, GridPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.text.TextFlow
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import GameEngine.Board
import Stone.Stone

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

  def switchToOptions() = {
    val loader = new FXMLLoader(getClass.getResource("options.fxml"))
    val root: Parent = loader.load()
    val stage = option.getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))
  }

  def exitMenu(): Unit = {
    val stage = label1.getScene.getWindow.asInstanceOf[Stage]
    stage.close()
    println("Shutting down...")
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
  @FXML
  private var winningLabel: Label= _

  // gameBoard!
  def randomPlay(): Unit = {

    //GameEngine.playRandomly()
    //atualizarEcra()
    //println("Jogada aleatória!") TEXTFLOW
  }

  def play(event: javafx.scene.input.MouseEvent): Unit = {
    val clickedNode = event.getPickResult.getIntersectedNode
    val colIndex = GridPane.getColumnIndex(clickedNode)
    val rowIndex = GridPane.getRowIndex(clickedNode)
    println("Mouse clicked on: " + rowIndex + ", " + colIndex)

    val coordenate = (rowIndex.toInt, colIndex.toInt)
    GameEngine.turno(coordenate) //fazer jogada

    val tabuleiro = GameEngine.gameStateAtual.board
    GameEngine.printBoard(tabuleiro)

    println("A atualizar ecrã...")
    //atualizar ecra
    atualizarEcra(tabuleiro, 0)
    print("Peça jogada!")
    if(GameEngine.winner != Stone.Empty){
      println("oi")
      switchToGameOver()
    }
  }

  def atualizarEcra(boardAtual: Board, rowIndex: Int): Unit={
    boardAtual match {
      case Nil => println("Ecrã atualizado!")
      case x::xs => {
        atualizarLinha(x, rowIndex, 0)
        atualizarEcra(xs, rowIndex+1)
      }
    }
  }

  def atualizarLinha(line: List[Stone], rowIndex: Int, colIndex: Int): Unit = {
    val scene = label2.getScene
    line match {
      case Nil => return
      case x::xs => {
        val circle = scene.lookup("#s"+rowIndex+colIndex).asInstanceOf[Circle]
        if(x == Stone.Black){
          circle.setFill(Color.BLACK)
          circle.setOpacity(1)
        } else if(x == Stone.White) {
          circle.setFill(Color.WHITE)
          circle.setOpacity(1)
        } else {
          val old= GameEngine.gameStateAtual
          GameEngine.gameStateAtual= GameState(old.toWin, old.captureWhite, old.captureBlack, old.board, (rowIndex, colIndex)::old.freeCoord, old.currentPlayer)
          circle.setOpacity(0)
        }
        atualizarLinha(xs, rowIndex, colIndex+1)
      }
    }
  }

  def switchToGameOver()={
    val loader = new FXMLLoader(getClass.getResource("gameOver.fxml"))
    val root: Parent = loader.load()
    val stage = label2.getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))

    println("yolo")
    if(GameEngine.winner == Stone.White){
      println("bruh")
      winningLabel.setText("White has won")
    }
    else{
      winningLabel.setText("Black has won!")
    }

  }

  def exitGame(): Unit = {
    GameEngine.currentThread.interrupt()
    Platform.exit()
    println("Shutting down...")
  }

  @FXML
  def undoJogada(): Unit = {
    GameEngine.undo()
    println("Jogada desfeita.")
    val tabuleiroAtualizado = GameEngine.gameStateAtual.board
    atualizarEcra(tabuleiroAtualizado, 0)
  }

  @FXML
  def resetJogo(): Unit = {
    println("Resetting game...")
    GameEngine.startGame() // Reinicia o estado do jogo
    atualizarEcra(GameEngine.gameStateAtual.board, 0)
  }

}