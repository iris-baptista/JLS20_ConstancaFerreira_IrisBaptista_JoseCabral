import Stone.Stone
import scala.annotation.tailrec

object GameEngine {
  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)

  //T1
  //ze :D
  //gerar uma coordenada aleatória
  //válida para a próxima jogada a partir da lista de posições livres fornecidas
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val sizeList = lstOpenCoords.size

    val (valorGerado, newRand) = rand.nextInt(sizeList) //gera numeros negativos?

    if (valorGerado > lstOpenCoords.size) {
      randomMove(lstOpenCoords, newRand)
    }
    else {
      (lstOpenCoords(valorGerado), newRand)
    }
  }

  //T2
  //devolve None se a cNova for invalida, devolve um board caso contrario
  //tb devolve uma lista de coordenadas livres
  def play(board: Board, player: Stone, cNova: Coord2D, cLivres: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    @tailrec
    def inList(restantes: List[Coord2D]): Boolean = { //verificar se cNova esta em cLivres
      restantes match {
        case Nil => false
        case x :: xs => //se so falta um, xs e Nil
          if (x == cNova) {
            true
          }
          else {
            inList(xs)
          }
      }
    }

    def removeC(oldList: List[Coord2D]): List[Coord2D] = { //devolve lista sem cNova
      oldList match {
        case Nil => Nil
        case x :: xs =>
          if (x != cNova) {
            x :: removeC(xs)
          }
          else {
            removeC(xs)
          }
      }
    }

    def addToBoard(oldBoard: Board): Board = { //devolve board com player
      val linha = cNova._1
      val coluna = cNova._2

      def searchBoard(remainingBoard: Board, currentLine: Int): Board = {
        remainingBoard match { //vai a procura da linha
          case Nil => Nil
          case x :: xs =>
            if (currentLine == linha) {
              val newLine = alterLine(x, 0)
              newLine :: xs
            }
            else {
              x :: searchBoard(xs, currentLine + 1)
            }
        }
      }

      def alterLine(l: List[Stone], currentCol: Int): List[Stone] = { //vai a procura da coluna
        l match {
          case Nil => Nil
          case x :: xs =>
            if (currentCol == coluna) {
              player :: xs
            }
            else {
              x :: alterLine(xs, currentCol + 1)
            }
        }
      }

      val novoBoard = searchBoard(oldBoard, 0) //assumir q comecamos a contar em 0
      novoBoard //devolve board com stone nova
    }

    if (inList(cLivres)) { //se tiver
      val newLivres = removeC(cLivres) //remover cNova de cLivres (nova lista a devolver)
      val newBoard = addToBoard(board) //adicionar stone ao board (novo board a devolver)

      (Some(newBoard), newLivres) //devia alterar o quadro de alguma forma?
    }
    else { //se nao tiver, devolver none e a lista como esta
      (None, cLivres)
    }
  }

  //T3
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D], f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)):
  (Board, MyRandom, List[Coord2D]) = {
    val (coord, newR) = f(lstOpenCoords, r) // validate
    val (optBoard, newLstOpenCoords) = play(board, player, coord, lstOpenCoords)
    val newBoard = optBoard.getOrElse(None)
    if (newBoard == None) {
      (board, newR, newLstOpenCoords)
    } else {
      val newBoard = optBoard.get
      (newBoard, newR, newLstOpenCoords)
    }
  }

  //T4
  def printBoard(board: Board): Unit = {
    board.foreach { row =>
      println(row.map {
        case Stone.Black => "B"
        case Stone.White => "W"
        case Stone.Empty => "-"
      }.mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    val board = List(
      List(Stone.Black, Stone.White, Stone.Empty, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.White, Stone.Black, Stone.Empty, Stone.Empty),
      List(Stone.White, Stone.Empty, Stone.Black, Stone.Empty, Stone.Empty),
      List(Stone.White, Stone.Empty, Stone.Black, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty))

    println("Board inicial")
    printBoard(board)
    println("")

    val lstOpenCoords: List[Coord2D] = List((0, 2), (0, 3), (0, 4), (1, 0), (1, 3), (1, 4), (2, 1), (2, 3),
        (2,4), (3, 1), (3, 3), (3,4), (4, 0), (4, 1), (4,2), (4,3), (4,4))

    val rand = new MyRandom(1L)
    val player1= Stone.Black
    val player2= Stone.White

    val (nextBoard, nextRand, nextLstOpenCoords) = playRandomly(board, rand, player1, lstOpenCoords, randomMove)
    println("Player 1 moves!")
    printBoard(nextBoard)
    println("")

    val (newBoard, newRand, newLstOpenCoords) = playRandomly(nextBoard, nextRand, player2, nextLstOpenCoords, randomMove)
    println("Player 2 moves!")
    printBoard(newBoard)
    println("")
  }
}
