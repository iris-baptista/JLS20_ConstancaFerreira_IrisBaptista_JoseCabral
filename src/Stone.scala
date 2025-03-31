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
    val (index, newRand) = rand.tirarRandao(lstOpenCoords.size) //isto gera de 0-(size-1)
    (lstOpenCoords(index), newRand) //devolve uma posicao valida como vem da lista!
  }

  //usando codigo do pwp
  //isto deve ser feito fora do stone??? N sei
  /*trait RandomT{ //nao sei o q traits fazem ainda sorry
    def nextInt: (Int, RandomT)
  }

  case class MyRandom() extends RandomT {
    def nextInt(): (Int, RandomT) = {
      val nextR= MyRandom()
      val nextI= 0

      (nextI, nextR)
    }
  }*/

  case class MyRandom() {
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
    var cLivres= List((1, 2), (3, 4), (4, 2), (1, 1), (2, 3), (2, 2), (6, 7), (6, 6))
    var rand= MyRandom()

    val (val1, newRand1) = randomMove(cLivres, rand)
    println(val1)
    val (val2, newRand2) = randomMove(cLivres, newRand1)
    println(val2)
    val (val3, newRand3) = randomMove(cLivres, newRand2)
    println(val3)
    val (val4, newRand4) = randomMove(cLivres, newRand3)
    println(val4)

    //samesies?
    val val12 = randomMove(cLivres, rand)
    println(val12)
    val val22 = randomMove(cLivres, rand)
    println(val22)
    val val32 = randomMove(cLivres, rand)
    println(val32)
    val val42 = randomMove(cLivres, rand)
    println(val42)
  }
}
