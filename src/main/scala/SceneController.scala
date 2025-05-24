/*import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label}
import javafx.application.Platform
import javafx.scene.layout.{AnchorPane, GridPane, VBox}
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.text.{Text, TextFlow}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import GameEngine.Board
import Stone.Stone

class SceneController {
  //Main Menu!
  @FXML
  private var label1: VBox = _
  @FXML
  private var gameTitle: Label = _
  @FXML
  private var startButton: Button = _
  @FXML
  private var option: Button = _

  //altera cor do titulo
  def changeColor() = {
    if(gameTitle.getTextFill.equals(Color.BLACK)){
      gameTitle.setTextFill(Color.WHITE)
    }
    else if(gameTitle.getTextFill.equals(Color.WHITE)){
      gameTitle.setTextFill(Color.BLACK)
    }
  }

  //muda de scene para a pagina do tabuleiro
  def switchToGame() = {
    val loader = new FXMLLoader(getClass.getResource("gameBoard.fxml"))
    val root: Parent = loader.load()
    val stage = startButton.getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))

    //inicia o jogo
    GameEngine.startGame()
  }

  //muda de scene para a pagina das options
  def switchToOptions() = {
    val loader = new FXMLLoader(getClass.getResource("options.fxml"))
    val root: Parent = loader.load()
    val stage = option.getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))
  }

  //fechar a janela
  def exitMenu(): Unit = {
    val stage = label1.getScene.getWindow.asInstanceOf[Stage]
    stage.close()
    println("Shutting down...")
  }

  //GameBoard!
  @FXML
  private var label2: AnchorPane = _
  @FXML
  private var textBox: TextFlow = _

  //jogada aleatoria
  def randomPlay(): Unit = {
    val coordenate= GameEngine.turnoRandom()
    val tabuleiro = GameEngine.gameStateAtual.board
    GameEngine.printBoard(tabuleiro) //para debugging/verificar q esta a funcionar

    //atualizar ecra
    atualizarEcra(tabuleiro, 0)
    textBox.getChildren.add(new Text("Peça jogada na posição "+coordenate+"\n"))

    if(GameEngine.winner != Stone.Empty){ //se alguem ganhou
      switchToGameOver()
    }
  }

  //fn para completar uma jogada
  def play(event: javafx.scene.input.MouseEvent): Unit = {
    //encontra a posicao q o user selecionou
    val clickedNode = event.getPickResult.getIntersectedNode
    val colIndex = GridPane.getColumnIndex(clickedNode)
    val rowIndex = GridPane.getRowIndex(clickedNode)
    //println("Mouse clicked on: " + rowIndex + ", " + colIndex)

    val coordenate = (rowIndex.toInt, colIndex.toInt)
    GameEngine.turno(coordenate) //fazer jogada

    val tabuleiro = GameEngine.gameStateAtual.board
    GameEngine.printBoard(tabuleiro)

    //atualizar ecra
    atualizarEcra(tabuleiro, 0)
    textBox.getChildren.add(new Text("Peça jogada na posição "+coordenate+"\n"))

    if(GameEngine.winner != Stone.Empty){
      switchToGameOver()
    }
  }

  //atualiza ecra basiado na matriz dada
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
        val circle = scene.lookup("#s"+rowIndex+colIndex).asInstanceOf[Circle] //encontra a peca q o utilizador selecionou
        if(x == Stone.Black){ //se a posicao e uma peca preta
          circle.setFill(Color.BLACK)
          circle.setStroke(Color.BLACK)
          circle.setOpacity(1) //fazer peca visivel
        }
        else if(x == Stone.White) {
          circle.setFill(Color.WHITE)
          circle.setStroke(Color.WHITE) //para pecas brancas nao terem a border preta
          circle.setOpacity(1)
        }
        else {
          circle.setOpacity(0) //peca fica "invisivel" (mas ainda da para ter interacao com a peca!

          //atualiza lista de coordenadas livres para incluir esta posicao
          val old= GameEngine.gameStateAtual
          GameEngine.gameStateAtual= GameState(old.toWin, old.captureWhite, old.captureBlack, old.board, (rowIndex, colIndex)::old.freeCoord, old.currentPlayer)
        }

        atualizarLinha(xs, rowIndex, colIndex+1)
      }
    }
  }

  @FXML //muda para o ultimo screen
  def switchToGameOver()={
    val loader = new FXMLLoader(getClass.getResource("gameOver.fxml"))
    val root: Parent = loader.load()
    val stage = label2.getScene.getWindow.asInstanceOf[Stage]
    stage.setScene(new Scene(root))

    // obter o controller associado ao gameOver.fxml
    val controller = loader.getController[SceneController]

    // alterar a label através do controller
    if (GameEngine.winner == Stone.White) {
      controller.winningLabel.setText("White has won")
    }
    else {
      controller.winningLabel.setText("Black has won!")
    }
  }

  //para fechar o programa
  def exitGame(): Unit = {
    GameEngine.currentThread.interrupt()
    Platform.exit()
    println("Shutting down...")
  }

  @FXML
  def undoJogada(): Unit = {
    GameEngine.undo()
    //println("Jogada desfeita.")
    val tabuleiroAtualizado = GameEngine.gameStateAtual.board //atualiza tabuleiro
    atualizarEcra(tabuleiroAtualizado, 0)

    textBox.getChildren.add(new Text("Undo Last Move\n"))
  }

  @FXML
  def resetJogo(): Unit = {
    GameEngine.startGame() // Reinicia o estado do jogo
    atualizarEcra(GameEngine.gameStateAtual.board, 0)

    textBox.getChildren.add(new Text("Game Reset\n"))
  }

  //GameOver
  @FXML
  private var winningLabel: Label = _
}*/