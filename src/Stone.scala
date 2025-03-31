import scala.util.Random

object Stone extends Enumeration{
  type Board = List[List[Stone]]
  type Coord2D = (Int,Int)

  type Stone = Value
  val Black,White,Empty = Value

  //T1
  //gerar uma coordenada aleatória
  //válida para a próxima jogada a partir da lista de posições livres fornecidas
  def randomMove(lstOpenCoords: List[Coord2D], rand:MyRandom):(Coord2D, MyRandom) = {
    val (index, newRand) = rand.tirarRandao(lstOpenCoords.size) //isto gera de 0-size ou de 0-(size-1)?
    (lstOpenCoords(index), newRand) //devolve uma posicao valida como vem da lista!
  }

  class MyRandom {
    def tirarRandao(max: Int): (Int, MyRandom) = {
      val randNum = Random.nextInt(max) // Gera um randu de 0 a (max - 1), q sera o size()
      (randNum, this)
    }
  }

  //T2
//  def play(board:Board, player:Stone, coord:Coord2D, lstOpenCoords:List[Coord2D]):(Option[Board], List[Coord2D]){
//
//  }

  //T3
//  def playRandomly(board:Board, r:MyRandom, player:Stone, lstOpenCoords:List[Coord2D], f:(List[Coord2D], MyRandom) => (Coord2D, MyRandom)):(Board,MyRandom, List[Coord2D]) {
//
//  }

  //T4
  /*def printBoard(board:Board): Unit = {

  } */

  def main(args: Array[String]): Unit = {
    println("amongus")
  }

}
