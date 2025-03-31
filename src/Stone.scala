import scala.util.Random

object Stone extends Enumeration{
  type Board = List[List[Stone]]
  type Coord2D = (Int,Int)

  type Stone = Value
  val Black,White,Empty = Value

  class MyRandom {
    def tirarRandao(max: Int): (Int, MyRandom) = {
      val randNum = Random.nextInt(max) // Gera um randu de 0 a (max - 1), q sera o size()
      (randNum, this)
    }
  }

  //T1
  //ze :D
  //gerar uma coordenada aleatória
  //válida para a próxima jogada a partir da lista de posições livres fornecidas

  //vou assumir que temos 8 coordenadas possiveis para serem selecionadas:
  //      0  0  0
  //      0  1  0
  //      0  0  0
  // nao sei usar o randão ;_;


  def randomMove(lstOpenCoords: List[Coord2D], rand:MyRandom):(Coord2D, MyRandom) = {
    val (index, newRand) = rand.tirarRandao(lstOpenCoords.size)
    (lstOpenCoords(index), newRand)

    //devem faltar exceçoes: se a coordenada for uma parece ou ja tiver ocupada
    // este codigo assume que a lista fornecida ja tem as coordenadas certas
    //isto nao está como poo, onde tu darias a coordenada propria da cena e ele gerava a lista das coordenadas possiveis
  }


  //T2
  def play(board:Board, player:Stone, coord:Coord2D, lstOpenCoords:List[Coord2D]):(Option[Board], List[Coord2D]) = {
    (None,lstOpenCoords)
  }

  //T3
  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D], f:(List[Coord2D], MyRandom) => (Coord2D, MyRandom)):
  (Board, MyRandom, List[Coord2D]) = {
    val (coord,newR) = f(lstOpenCoords,r) // validate
    val (optBoard,newLstOpenCoords) = play(board, player, coord, lstOpenCoords)
    val newBoard = optBoard.getOrElse(None)
    if(newBoard == None) {
      (board, newR, newLstOpenCoords)
    } else {
      val newBoard = optBoard.get
      (newBoard, newR, newLstOpenCoords)
    }
  }

  //T4
  def printBoard(board:Board): Unit = {
    board.foreach { row =>
      println(row.map{
        case Black => "B"
        case White => "W"
        case Empty => "-"
      }.mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    val board = List(List(Black, White, Empty),List(Empty,White,Black), List(White,Empty,Black))
    printBoard(board)
    //playRandomly(board, )
  }
}
