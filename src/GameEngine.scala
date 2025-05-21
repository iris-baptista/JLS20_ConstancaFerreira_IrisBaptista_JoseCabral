import Stone.Stone
import scala.annotation.tailrec

object GameEngine {


  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)

  type GameState = (Board, List[Coord2D])
  var jogadas: List[GameState] = List()

  //T1
  //ze :D
  //gerar uma coordenada aleatória
  //válida para a próxima jogada a partir da lista de posições livres fornecidas

  //vou assumir que temos 8 coordenadas possiveis para serem selecionadas:
  //      0  0  0
  //      0  1  0
  //      0  0  0
  // nao sei usar o randão ;_;

  def randomMove(lstOpenCoords: List[Coord2D], rand: Random): (Coord2D, Random) = {
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

    //novaJogada(board, lstOpenCoords)
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



  //T5

  //T6

  //os acima sao da concha :D

  //T7

  def timer(): Unit = {
    val inicio = System.currentTimeMillis()
    val joever = 15 * 1000 // 15 segundos em milissegundos pq ya

    @tailrec //confio q ponho aqui
    def espera(): Unit = {
      val agora = System.currentTimeMillis()
      if (agora - inicio < joever) {
        espera() // chamada recursiva da cena
      }
    }
    println("timer iniciado confia")
    espera()
    println("its joever")

  }




  //System.currentTimeMillis()

  //def timerUp()
  //ainda nao sei o q por aqui





  def novaJogada(board: Board, livres: List[Coord2D]): Unit = {
    //por esta cena antes de cada jogada:   novaJogada(board, lstOpenCoords)
    jogadas = (board, livres) :: jogadas
  }

  //achei q podia usar folding, o chat discordou ;_;
  def undo(): Option[GameState] = { //mas dava para deixar tail recursive
    jogadas match {
      case Nil =>
        println("nao da zezoca")
        None
      case anterior :: restantes =>
        jogadas = restantes
        Some(anterior)
    }
  }


  //T8

  def main(args: Array[String]): Unit = {
        val lstOpenCoords: List[Coord2D] = List(
          (0, 0), (0, 1), (0, 2),
          (1, 0), (1, 2),
          (2, 0), (2, 1), (2, 2)
        )

        val rand = new MyRandom(1L)
        val size = lstOpenCoords.size

        val (valor1, nextR) = rand.nextInt(size)
        val (valor2, tR) = nextR.nextInt(size)
        val (valor3, fR) = tR.nextInt(size)
        val (valor4, fiveR) = fR.nextInt(size)

        //println(valor1)
        //println(valor2)
        //println(valor3)
        //println(valor4)

        val (coord1, newRand) = randomMove(lstOpenCoords, rand)
        val (coord2, nextRand) = randomMove(lstOpenCoords, rand) //igual ao 1
        val (coord3, _) = randomMove(lstOpenCoords, nextRand) //differente dos dois

        println(s"Primeira coordenada aleatória escolhida: $coord1")
        println(s"Segunda coordenada aleatória escolhida: $coord2")
        println(s"Terceira coordenada aleatória escolhida: $coord3")


        val board = List(List(Stone.Black, Stone.White, Stone.Empty),
          List(Stone.Empty, Stone.White, Stone.Black),
          List(Stone.White, Stone.Empty, Stone.Black))

    printBoard(board)

    println("timer ligado")
    timer()
    println("wake up joe")

    //novaJogada(board, lstOpenCoords)

  }
}

