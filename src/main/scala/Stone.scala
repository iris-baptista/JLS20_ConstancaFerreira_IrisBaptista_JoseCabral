import  scala.util.Random

//projeto realizado por José Cabral nº110864, Constança nº, Íris Baptista nº

object Stone extends Enumeration{
  type Board = List[List[Stone]]
  type Coord2D = (Int, Int) //(row, column)

  type Stone = Value
  val Black, White, Empty = Value

  //T1

  //ze :D
  //gerar uma coordenada aleatória
  //válida para a próxima jogada a partir da lista de posições livres fornecidas

  //vou assumir que temos 8 coordenadas possiveis para serem selecionadas:
  //      0  0  0
  //      0  1  0
  //      0  0  0
  // nao sei usar o randão ;_;

  class MyRandom {
    def tirarRandao(max: Int): (Int, MyRandom) = {
      val randNum = Random.nextInt(max) // Gera um randu de 0 a (max - 1), q sera o size()
      (randNum, this)
    }
  }
  def randomMove(lstOpenCoords: List[Coord2D], rand:MyRandom):(Coord2D, MyRandom) = {
    val (index, newRand) = rand.tirarRandao(lstOpenCoords.size)
    (lstOpenCoords(index), newRand)

    //devem faltar exceçoes: se a coordenada for uma parece ou ja tiver ocupada
    // este codigo assume que a lista fornecida ja tem as coordenadas certas
    //isto nao está como poo, onde tu darias a coordenada propria da cena e ele gerava a lista das coordenadas possiveis
  }



  //T2

  //T3
  //  def playRandomly(board: Board, r: MyRandom, player: Stone,
  //                   lstOpenCoords: List[Coord2D], f: (List[Coord2D], MyRandom) =>
  //    (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D]) {
  //
  //  }

  //T4
  //  def printBoard():{
  //
  //  }

  //among
  def main(args: Array[String]): Unit = {
    val lstOpenCoords: List[Coord2D] = List(
      (0, 0), (0, 1), (0, 2),
      (1, 0),         (1, 2),
      (2, 0), (2, 1), (2, 2)
    )//xd

    val rand = new MyRandom()

    val (coord, _) = randomMove(lstOpenCoords, rand)

    println(s"Coordenada aleatória escolhida: $coord")
  }


}