import Stone.{Black, Board, Coord2D, Empty, White}

import scala.annotation.tailrec
import scala.util.Random

object Engine {
  //T1
  //ze :D
  //gerar uma coordenada aleatória
  //válida para a próxima jogada a partir da lista de posições livres fornecidas

  //vou assumir que temos 8 coordenadas possiveis para serem selecionadas:
  //      0  0  0
  //      0  1  0
  //      0  0  0
  // nao sei usar o randão ;_;

  def getLeftMost(num: Int): Int = {
    if(num < 10){
      return num
    }
    else{
      getLeftMost(num/10)
    }
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: Random): (Coord2D, Random) = {
    val (valorGerado, newRand) = rand.nextInt
    def index= getLeftMost(valorGerado)

    if(index > lstOpenCoords.size || index < 0){
      randomMove(lstOpenCoords, newRand)
    }
    else{
      (lstOpenCoords(index), newRand)
    }
  }



    //devem faltar exceçoes: se a coordenada for uma parede ou ja tiver ocupada
    // este codigo assume que a lista fornecida ja tem as coordenadas certas
    //isto nao está como poo, onde tu darias a coordenada propria da cena e ele gerava a lista das coordenadas possiveis

    // 3-4-25 a constança disse q o codigo nao tava "puro", pus no chat e arranjeu :D

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
    board.foreach {
      row =>
      println(row.map {
        case Black => "B"
        case White => "W"
        case Empty => "-"
      }.mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    //    val lstOpenCoords: List[Coord2D] = List(
    //      (0, 0), (0, 1), (0, 2),
    //      (1, 0), (1, 2),
    //      (2, 0), (2, 1), (2, 2)
    //    ) //xd
    //
    //    val rand = new MyRandom()
    //
    //    val (coord, _) = randomMove(lstOpenCoords, rand)
    //
    //    println(s"Coordenada aleatória escolhida: $coord")
    //println("ze")


    val board = Board.empty
    val rand = new MyRandom()

    val lstOpenCoords: List[Coord2D] = List(
      (0, 0), (0, 1), (0, 2),
      (1, 0), (1, 2),
      (2, 0), (2, 1), (2, 2)
    )

    val (coord, _) = randomMove(lstOpenCoords, rand)
    println(s"Coordenada aleatória escolhida: $coord")

    val (newBoard, _, newLstOpenCoords) = playRandomly(board, rand, Black, lstOpenCoords, randomMove)
    printBoard(newBoard)
  }
}