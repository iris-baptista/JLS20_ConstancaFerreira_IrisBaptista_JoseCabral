import Stone.Stone
import scala.annotation.tailrec

object GameEngine {
  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)
  var jogadas: List[GameState] = List()
  var peca: Boolean = true //vou usar isto para mudar das peças brancas para as pretas e vice versa

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
    board match {
      case x::Nil =>{
        printLine(x)
        println("")
      }
      case x::xs => {
        printLine(x)
        println("")
        printBoard(xs)
      }
    }
  }

  def printLine(list: List[Stone]): Any = {
    list match {
      case x::Nil =>{
        if(x.toString == "Black") {
          print(" B ")
        } else if(x.toString == "White") {
          print(" W ")
        } else if(x.toString == "Empty") {
          print(" - ")
        }
      }
      case x::xs =>{
        if(x.toString == "Black") {
          print(" B ")
          printLine(xs)
        } else if(x.toString == "White") {
          print(" W ")
          printLine(xs)
        } else if(x.toString == "Empty") {
          print(" - ")
          printLine(xs)
        }
      }
    }
  }

  //T5
  //captura pecas (se der para capturar)
  //devolve um novo tabuleiro sem a(s) peça(s) capturadas (o espaco fica vazio) e o número de peças capturadas
  //player e a peca jogada, coordenada e onde foi jogada
  def getGroupStones(board: Board, player: Stone, playerCoord: Coord2D) : (Board, Int) = {
    //fns para recursao
    //devolve pecas q estao a volta da coord passada
    def getSurroundingStones(coord: Coord2D): List[Coord2D] = {
      @tailrec //verifica q as posicoes estao validas
      def getValidCoord(currentPossible: List[Coord2D], valid: List[Coord2D]) : List[Coord2D] = {
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

    @tailrec //encontra todas as pecas do outro jogador a volta da peca jogada (pode ter mais q uma)
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
    //primeira lista devolvida contem as liberties do grupo, segunda contem todas as pedras do grupo
    def findGroup(currentStones: List[Coord2D], liberties: List[Coord2D], stones: List[Coord2D]): (List[Coord2D], List[Coord2D]) = {
      currentStones match { //current stones so tem stones do opponent
        case Nil => (liberties, stones) //se ja nao ha mais para visitar
        case x::xs => {
          if(stones.contains(x)){ //se estamos a repetir uma pedra na recursao q ja verificamos
            findGroup(xs, liberties, stones) //passar atual e continuar recursao
          }
          else{
            val surrounding= getSurroundingStones(x) //encontrar pecas a volta da atual (opponent)
            val otherMembers= findOpponent(surrounding, List()) //encontrar todas as posicoes q nao tem uma peca igual a atual
            val libertiesFound= getLiberties(surrounding, List()) //nas pecas iguais marcar as liberdades

            //repetir chamada juntado pecas q podem ser parte do grupo ao currentStones, e atualizando as liberties/stones
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
          if(stone == player || stone == Stone.Empty){ //se nao for o apponent
            getLiberties(xs, x::currentLiberties) //adicinar a lista de liberties e continua a percurrer as coordenadas dadas
            //assim adiciouna espacos vazios e espacos com o proprio jogador
          }
          else{ //caso contrario continuar a percurrer
            getLiberties(xs, currentLiberties)
          }
        }
      }
    }

    //junta as listas dadas de forma a nao ter valores repetidos
    def joinLists(newL: List[Coord2D], currentL: List[Coord2D]): List[Coord2D] = {
      newL match{
        case Nil => currentL
        case x::xs => {
          if(currentL.contains(x)){ //se ja tem o valor
            joinLists(xs, currentL) //passa valor e continuar a percurrer
          }
          else{ //se nao tem o valor concatena a coordenada
            joinLists(xs, x::currentL)
          }
        }
      }
    }

    @tailrec //verifica se as liberties foram todas preenchidas pelo jogador (se capturou o grupo)
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

    @tailrec //tira as pecas dadas do ecra
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

    //verifica cada grupo de pecas do adversario para ver se foram capturados
    def checkGroups(possibleGroups: List[Coord2D], workingBoard: Board, currentCaptured: Int): (Board, Int) = {
      possibleGroups match{
        case Nil => (workingBoard, currentCaptured) //se nao encontrou nenhum grupo capturado
        case x::xs => {
          val (liberties, stones)= findGroup(List(x), List(), List())
          val captured= checkLiberties(liberties)

          if(captured){
            val novoBoard= atualizarBoard(workingBoard, stones)
            val total= stones.size

            checkGroups(xs, novoBoard, total+currentCaptured)
          }
          else{ //se nao capturou as pecas, devolve como estava
            checkGroups(xs, workingBoard, currentCaptured)
          }
        }
      }
    }

    //codigo principal
    val surrounding= getSurroundingStones(playerCoord)
    val opponents= findOpponent(surrounding, List())
    checkGroups(opponents, board, 0)
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

  def turno(c:Coord2D, gameState: GameState, player:Stone):GameState= {
    val novoTabuleiro = gameState.board
    val coorLivres = gameState.freeCoord

    val (novaPlay, novasLivres) = play(novoTabuleiro, player, c, coorLivres)
    val optBoard = novaPlay.getOrElse(None)
    if (optBoard != None) {
      val tpoBoard = novaPlay.get
      val (capturedBoard, captured) = getGroupStones(tpoBoard, player, c)

      if (player == Stone.White) {
        val novoGameState = novaJogada(gameState.toWin, gameState.captureWhite + captured, gameState.captureBlack, capturedBoard, novasLivres)
        val optStone = seGanhou(novoGameState)
        val winner = optStone.getOrElse(None)
        if (winner != None) {
          println("u suck")
        } else {
          //changeTurno()
        }

      } else {
        val novoGameState = novaJogada(gameState.toWin, gameState.captureWhite, gameState.captureBlack + captured, capturedBoard, novasLivres)
        val optStone = seGanhou(novoGameState)
        val winner = optStone.getOrElse(None)
        if (winner != None) {
          println("u suck")
        } else {
          //changeTurno()
        }

      }
  }

  def newBoard(coluna:Int, tempBoard:Board, tempList:List[Coord2D], linha:Int): (Board,List[Coord2D]) = {

    if(linha>0){

      newBoard(coluna, createLine(coluna, Nil):: tempBoard,createCoor(coluna,tempList,linha ),linha-1)
    }else{
      (tempBoard,tempList)
    }

  }

  def createLine(a:Int, temp: List[Stone]):List[Stone] = {
    if( a > 0){
      createLine(a-1, Stone.Empty :: temp )

    }else {
      temp

    }

  }

  def createCoor(coluna: Int, temp:List[Coord2D], linha:Int): List[Coord2D]  = {
    if( coluna > 0){
      createCoor(coluna-1, (linha-1,coluna-1)::temp, linha)

    }else {
      temp

    }
  }

  //achei q podia usar folding, o chat discordou ;_;
  def undo(): Option[GameState] = { //mas dava para deixar tail recursive
    jogadas match {
      case Nil =>
        println("nao da zezoca")
        None
      case anterior :: restantes =>
        jogadas = restantes
        Some(anterior) //faz a cena
    }
  }

  def novaJogada(toWin: Int, captureWhite: Int, captureBlack: Int, board: Board, livres: List[Coord2D]): Unit = {
    //BUEDA IMPORTANTE
    val estadoAtual = GameState(toWin, captureWhite, captureBlack, board, livres)
    jogadas = estadoAtual :: jogadas // Adiciona à head da lista (mais recente primeiro)

    estadoAtual
  }

    def timer(): Unit = { //versão mais fixe
      val t = new Thread() {
        override //override para este exemplo especifico
        def run(): Unit = {
          val inicio = System.currentTimeMillis()
          val joever = 15 * 1000

          println("timer iniciado confia")

          @tailrec
          def espera(): Unit = { // espera até os 15s passarem
            val agora = System.currentTimeMillis()
            if (agora - inicio < joever) {
              espera()
            }
          }

          espera()
          println("its joever")
          //changeTurno()
        }
      }

      t.start()
    }

  def main(args: Array[String]): Unit = {
    val board = List(
      List(Stone.Black, Stone.White, Stone.Empty, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.White, Stone.Black, Stone.Empty, Stone.Empty),
      List(Stone.White, Stone.Empty, Stone.Black, Stone.Empty, Stone.Empty),
      List(Stone.White, Stone.Empty, Stone.Black, Stone.Empty, Stone.Empty),
      List(Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty, Stone.Empty))

    println("Board Inicial:")
    printBoard(board)
    println("")

    val lstOpenCoords: List[Coord2D] = List((0, 2), (0, 3), (0, 4), (1, 0), (1, 3), (1, 4), (2, 1), (2, 3),
        (2,4), (3, 1), (3, 3), (3,4), (4, 0), (4, 1), (4,2), (4,3), (4,4))

    val rand = new MyRandom(1L)
    val player1= Stone.Black
    val player2= Stone.White

    val (nextBoard, nextRand, nextLstOpenCoords) = playRandomly(board, rand, player1, lstOpenCoords, randomMove)
    println("Player 1 Moves!")
    printBoard(nextBoard)
    println("")

    val (newBoard, newRand, newLstOpenCoords) = playRandomly(nextBoard, nextRand, player2, nextLstOpenCoords, randomMove)
    println("Player 2 Moves!")
    printBoard(newBoard)
  }
}
