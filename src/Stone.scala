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
//def randomMove(lstOpenCoords:List[Coord2D], rand:MyRandom):(Coord2D,MyRandom){
//  val (index, newRand) = rand.tirarRandao(lstOpenCoords.size)
//  (lstOpenCoords(index), newRand)
//  // devem faltar exceçoes: se a coordenada for uma parece ou ja tiver ocupada
//  // este codigo assume que a lista fornecida ja tem as coordenadas certas
//  // isto nao está como poo, onde tu darias a coordenada propria da cena e ele gerava a lista das coordenadas possiveis
//}

  //T2
//  def play(board:Board, player:Stone, coord:Coord2D, lstOpenCoords:List[Coord2D]):(Option[Board], List[Coord2D]){
//
//  }

  //T3
//  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D], f:(List[Coord2D], MyRandom) => (Coord2D, MyRandom)):(Board,MyRandom, List[Coord2D]) {
//    //randomMove(lstOpenCoords,r) validate
//    //play(board, player,randomMove(lstOpenCoords,r)._1,lstOpenCoords) generate new board
//    //remove from lstOpenCoords the position given by T1
//  }

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
    printBoard(List(List(Black, White, Empty),List(Empty,White,Black), List(White,Empty,Black)))
  }
}
