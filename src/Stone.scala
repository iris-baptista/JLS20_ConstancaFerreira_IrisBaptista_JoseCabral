import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Stone extends Enumeration{
  type Stone = Value
  val Black,White,Empty = Value

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)

  //T1
//  def randomMove(lstOpenCoords:List[Coord2D], rand:MyRandom):(Coord2D,MyRandom){
//
//  }

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
//  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D], f:(List[Coord2D], MyRandom) => (Coord2D, MyRandom)):(Board,MyRandom, List[Coord2D]) {
//
//  }

  //T4
  /*def printBoard(board:Board): Unit = {

  }*/

  def displayM(m: Board): Unit = {
    m match {
      case Nil => return
      case x :: xs => {
        displayLine(x)
        println()
        displayM(xs)
      }
    }

    println()

    def displayLine(l: List[Stone]): Unit = {
      l match {
        case Nil => return
        case x :: xs => {
          print(x)
          print(" ")
          displayLine(xs)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //testes
    val board = List(List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty)) //cria um 5x5
    val player = Stone.Black
    //val cNova= (0,1)
    val cLivres = List((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (2, 0),
      (2, 1), (2, 2), (2, 3), (2, 4), (3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (4, 0), (4, 1), (4, 2), (4, 3), (4, 4))

    /*val (newBoard, newLivres)= Stone.play(board, player, cNova, cLivres)
    println(newLivres)
    println(newBoard.getOrElse(None))*/

    val (nextB1, nextL1) = Stone.play(board, player, (0, 1), cLivres)
    //println(nextB1.getOrElse(None))
    displayM(nextB1.get)

    val (nextB2, nextL2) = Stone.play(nextB1.get, player, (0, 1), nextL1)
    println(nextB2.getOrElse(None))
    //displayM(nextB2.get)

    val (nextB3, nextL3) = Stone.play(nextB1.get, player, (2, 2), nextL1)
    //println(nextB3.getOrElse(None))
    displayM(nextB3.get)

    val (nextB4, nextL4) = Stone.play(nextB3.get, player, (4, 3), nextL3)
    //println(nextB4.getOrElse(None))
    displayM(nextB4.get)

    val (nextB5, nextL5) = Stone.play(nextB4.get, player, (1, 2), nextL4)
    //println(nextB5.getOrElse(None))
    displayM(nextB5.get)

    val (nextB6, nextL6) = Stone.play(nextB5.get, player, (7, 6), nextL5)
    println(nextB6.getOrElse(None))
    //displayM(nextB6.get)
  }

}
