object Stone extends Enumeration{
  type Board = List[List[Stone]]
  type Coord2D = (Int,Int)

  type Stone = Value
  val Black,White,Empty = Value

  //T1
//  def randomMove(lstOpenCoords:List[Coord2D], rand:MyRandom):(Coord2D,MyRandom){
//
//  }

  //T2
//  def play(board:Board, player:Stone, coord:Coord2D, lstOpenCoords:List[Coord2D]):(Option[Board], List[Coord2D]){
//
//  }

  //T3
//  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D], f:(List[Coord2D], MyRandom) => (Coord2D, MyRandom)):(Board,MyRandom, List[Coord2D]) {
//
//  }

  //T4
  def printBoard(board:Board): Unit = {

  }

  def main(args: Array[String]): Unit = {
    println("amongus")
  }

}
