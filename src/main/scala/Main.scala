import Stone.Stone
import scala.annotation.tailrec
object Main {
  def main(args: Array[String]) {
    print("Peças a capturar para ganhar: ")
    val numeroDeCapturas= scala.io.StdIn.readInt() //pede numero de capturas
    println("")

    GameEngine.startGame()
    while(GameEngine.winner == Stone.Empty){
      print("Quer fazer uma jogada aleatoria? (true/false): ")
      val playRandom= scala.io.StdIn.readBoolean()
      println("")

      if(playRandom){
        val rand= MyRandom(System.currentTimeMillis())
        GameEngine.turnoRandom()
        GameEngine.printBoard(GameEngine.gameStateAtual.board)
      }
      else{
        print("Row index (começa a contar por 0): ")
        val coordX= scala.io.StdIn.readInt()
        println("")
        print("Column index (começa a contar por 0): ")
        val coordY= scala.io.StdIn.readInt()
        println("")
        val coord= (coordX, coordY)

        GameEngine.turno(coord)
        GameEngine.printBoard(GameEngine.gameStateAtual.board)
      }
    }

    if(GameEngine.winner == Stone.Black){
      println("Black is the winner!")
    }
    else{ //se for o jogador branco q ganhou
      println("White is the winner!")
    }
  }

}

