import Stone.Stone
import scala.annotation.tailrec

object GameEngine {
  type Board = List[List[Stone]]
  type Coord2D = (Int, Int) //row, column

  //T1
  //gerar uma coordenada aleatória
  //válida para a próxima jogada a partir da lista de posições livres fornecidas
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val sizeList = lstOpenCoords.size
    val (valorGerado, newRand) = rand.nextInt(sizeList)

    (lstOpenCoords(valorGerado), newRand)
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

  //T5
  //devolve um novo tabuleiro sem peça(s) capturadas (i.e. substiuíndo-as por Stone.Empty) e o número de peças capturadas
  //isto e a fn de quando capturamos umas pecas, elas desaparecem do tabuleiro
  //player e a peca jogada, coordenada e onde foi jogada
  //deve atualizar o gameState???????????
  def getGroupStones(board: Board, player: Stone, playerCoord: Coord2D) : (Board, Int) = {
    //fns
    //devolve pecas a volta da coord passada
    def getSurroundingStones(coord: Coord2D): List[Coord2D] = {
      @tailrec
      def getValidCoord(currentPossible: List[Coord2D], valid: List[Coord2D]) : List[Coord2D] = { //verifica q as posicoes estao validas
        currentPossible match {
          case Nil => return valid
          case x :: xs => {
            val linha = x._1
            val coluna = x._2

            if(linha > -1 && linha < board.size && coluna > -1 && coluna < board.head.size){
              getValidCoord(xs, x::valid)
            }
            else{
              getValidCoord(xs, valid)
            }
          }
        }
      }

      val linha = coord._1
      val coluna = coord._2

      val possible= List((linha-1, coluna), (linha, coluna-1), (linha, coluna+1), (linha+1, coluna))
      getValidCoord(possible, List())
    }

    //encontra todas as pecas do outro jogador a volta da peca jogada (pode ter mais q uma)
    @tailrec
    def findOpponent(surroundingCoord: List[Coord2D], currentCoord: List[Coord2D]): List[Coord2D] = {
      surroundingCoord match{
        case Nil => currentCoord
        case x :: xs => {
          val linha = x._1
          val coluna = x._2

          val stone= board(linha)(coluna)
          if(stone != Stone.Empty && stone != player){ //se for o opponent
            findOpponent(xs, x::currentCoord) //addiciona a coordenada a lista antes de continuar a percurrer
          }
          else{ //caso contrario continua a verificar as outras coordenadas
            findOpponent(xs, currentCoord)
          }
        }
      }
    }

    //encontra um grupo de pecas do opponent, e as liberdades do grupo
    def findGroup(currentStones: List[Coord2D], liberties: List[Coord2D], stones: List[Coord2D]): (List[Coord2D], List[Coord2D]) = { //primeira lista devolvida contem as liberties do grupo, segunda contem todas as pedras do grupo
      currentStones match { //assumir q current stones so tem stones do opponent
        case Nil => (liberties, stones)
        case x::xs => {
          if(stones.contains(x)){ //se estamos a repetir uma pedra na recursao
            findGroup(xs, liberties, stones)
          }
          else{
            val surrounding= getSurroundingStones(x)
            val otherMembers= findOpponent(surrounding, List())
            val libertiesFound= getLiberties(surrounding, List())

            findGroup(joinLists(xs,otherMembers), joinLists(libertiesFound, liberties), x::stones)
          }
        }
      }
    }

    //encontra as coordenadas das liberties a volta de um grupo, dado as coordenadas das pecas no grupo
    def getLiberties(surroundingCoord: List[Coord2D], currentLiberties: List[Coord2D]) : List[Coord2D] = {
      surroundingCoord match{
        case Nil => currentLiberties
        case x :: xs => {
          val linha = x._1
          val coluna = x._2

          val stone= board(linha)(coluna)
          if(stone != Stone.White){ //se nao for o apponent
            getLiberties(xs, x::currentLiberties) //adicinar a lista de liberties e continua a percurrer as coordenadas dadas
          }
          else{ //caso contrario continuar a percurrer
            getLiberties(xs, currentLiberties)
          }
        }
      }
    }

    //junta as listas dadas para nao ter valores repetidos
    def joinLists(newL: List[Coord2D], currentL: List[Coord2D]): List[Coord2D] = {
      newL match{
        case Nil => currentL
        case x::xs => {
          if(currentL.contains(x)){ //se ja tem o valor
            joinLists(xs, currentL)
          }
          else{ //se nao tem o valor concatena a coordenada
            joinLists(xs, x::currentL)
          }
        }
      }
    }

    //verifica se as liberties foram todas preenchidas pelo jogador (se capturou o grupo)
    @tailrec
    def checkLiberties(coordenadasLeft: List[Coord2D]): Boolean = {
      coordenadasLeft match {
        case Nil => true //se todas tinham pecas do jogador
        case x::xs =>
          val linha= x._1
          val coluna= x._2

          val liberty= board(linha)(coluna)
          if(liberty != player){ //se nao tem uma peca do jogador
            false
          }
          else{
            checkLiberties(xs)
          }
      }
    }

    //tira as pecas capturadas do ecra
    //devia tb atualizar a lista de coordenadas livres para nao incluir as capturadas
    @tailrec
    def atualizarBoard(currentBoard: Board, coordenadas: List[Coord2D]): Board = {
      coordenadas match{
        case Nil => currentBoard
        case x::xs => {
          val linha= x._1
          val coluna= x._2

          def removeStone(remainingBoard: Board, currentLine: Int): Board = {
            remainingBoard match { //vai a procura da linha
              case Nil => Nil
              case y :: ys =>
                if (currentLine == linha) {
                  val newLine = alterLine(y, 0)
                  newLine :: ys
                }
                else {
                  y :: removeStone(ys, currentLine+1)
                }
            }
          }

          def alterLine(line: List[Stone], currentCol: Int): List[Stone] = {
            line match {
              case Nil => Nil
              case y :: ys =>
                if (currentCol == coluna) {
                  Stone.Empty :: ys
                }
                else {
                  y :: alterLine(ys, currentCol+1)
                }
            }
          }

          atualizarBoard(removeStone(currentBoard, 0), xs)
        }
      }
    }

    def checkGroups(possibleGroups: List[Coord2D]): (Board, Int) = {
      possibleGroups match{
        case Nil => (board, 0) //se nao encontrou nenhum grupo capturado
        case x::xs => {
          val (liberties, stones)= findGroup(List(x), List(), List()) //TESTADO
          val captured= checkLiberties(liberties) //TESTADO

          if(captured){
            val novoBoard= atualizarBoard(board, stones)
            val total= stones.size

            (novoBoard, total)
          }
          else{ //se nao capturou as pecas, devolve como estava
            checkGroups(xs)
          }
        }
      }
    }

    //codigo principal
    val surrounding= getSurroundingStones(playerCoord) //TESTADO
    val opponents= findOpponent(surrounding, List()) //TESTADO
    checkGroups(opponents)
  }

  //T6
  //verifica se o computador ou o jogador ganhou o jogo (ou seja, se já foram capturadas o número de peças necessárias)
  def seGanhou(gameState: GameState): Option[Stone] = {
    val toWin= gameState.toWin
    val captureW= gameState.captureWhite
    val captureB= gameState.captureBlack

    if(captureW == toWin){ //se branco ganhou
      return Some(Stone.White) //vou assumir q devolvemos a stone para o jogador q ganho
    }

    if(captureB == toWin){ //se o preto ganhou
      return Some(Stone.Black)
    }

    //se nao encontrou
    None //nao precisa da palavra return
  }

  def main(args: Array[String]): Unit = {
    val board = List(
      List(Stone.Black, Stone.White, Stone.Empty, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.White, Stone.Black, Stone.Empty, Stone.Empty),
      List(Stone.White, Stone.Black, Stone.Empty, Stone.Empty, Stone.Empty),
      List(Stone.White, Stone.Empty, Stone.Black, Stone.Empty, Stone.Black),
      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Black, Stone.White))

    println("Board Inicial:")
    printBoard(board)
    println("")

    val lstOpenCoords: List[Coord2D] = List((0, 2), (0, 3), (0, 4), (1, 0), (1, 3), (1, 4), (2, 1), (2, 3),
        (2,4), (3, 1), (3, 3), (3,4), (4, 0), (4, 1), (4,2), (4,3), (4,4)) //!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    val rand = new MyRandom(1L)
    val player1= Stone.Black
    val player2= Stone.White

    /*val (nextBoard, nextRand, nextLstOpenCoords) = playRandomly(board, rand, player1, lstOpenCoords, randomMove)
    println("Player 1 Moves!")
    printBoard(nextBoard)
    println("")

    val (newBoard, newRand, newLstOpenCoords) = playRandomly(nextBoard, nextRand, player2, nextLstOpenCoords, randomMove)
    println("Player 2 Moves!")
    printBoard(newBoard)*/

    val (board1, total1)= getGroupStones(board, player1, (0, 0)) //deve devolver o board antigo e 0
    println("Board1: "+total1)
    printBoard(board1)
    println("")

    val (board2, total2)= getGroupStones(board, player1, (4, 3)) //deve devolver o board novo e 1
    println("Board2: "+total2)
    printBoard(board2)
    println("")
  }
}
