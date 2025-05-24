import Stone.Stone
import scala.annotation.tailrec

object GameEngine {
  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)

  //initializar valores
  var jogadas: List[GameState] = List() //lista de gamestates ao longo do jogo (head contem o atual)
  var gameStateAtual = GameState(0, 0, 0, Nil, Nil, Stone.Empty) //gamestate do jogo atual
  var currentThread = new Thread() //o timer atual, initializado por um Thread() para nao comecar a contagem
  var winner= Stone.Empty //vazio enquanto ninguem ganhou
  var tempoDeJogada = 15 //tempo para cada jogada em segundos
  var numeroDeCapturas = 5 //numero de pecas q tem de capturar para ganhar

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
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D], f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Board, MyRandom, List[Coord2D]) = {
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
  //se der devolve um novo tabuleiro sem a(s) peça(s) capturadas (espaco fica vazio) e o número de peças capturadas
  //se nao capturar nada devolve o tabuleiroAtual e 0 capturas
  def getGroupStones(board: Board, player: Stone, playerCoord: Coord2D) : (Board, Int) = {  //coordenada e onde peca foi jogada
    //fns para recursao
    //devolve pecas q estao a volta da coord passada
    def getSurroundingStones(coord: Coord2D): List[Coord2D] = {
      @tailrec //verifica q as posicoes estao validas
      def getValidCoord(currentPossible: List[Coord2D], valid: List[Coord2D]) : List[Coord2D] = {
        currentPossible match {
          case Nil => valid
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

      val possible= List((linha-1, coluna), (linha, coluna-1), (linha, coluna+1), (linha+1, coluna)) //posicoes diretamente a volta da peca jogada
      getValidCoord(possible, List()) //para nao devolver posicoes fora do tabuleiro (invalidas)
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

    //encontra o grupo de pecas de um opponent, e as liberdades desse grupo
    //primeira lista devolvida contem as liberties do grupo, segunda contem todas as pedras do grupo
    def findGroup(currentStones: List[Coord2D], liberties: List[Coord2D], stones: List[Coord2D]): (List[Coord2D], List[Coord2D]) = {
      currentStones match { //current stones so vai ter stones do opponent entao nao temos de validar/verificar
        case Nil => (liberties, stones) //se ja nao ha mais para visitar
        case x::xs => {
          if(stones.contains(x)){ //se estamos a repetir uma pedra do grupo q ja verificamos
            findGroup(xs, liberties, stones) //ignorar x e continuar recursao
          }
          else{
            val surrounding= getSurroundingStones(x) //encontrar pecas a volta do opponent atual (x)
            val otherMembers= findOpponent(surrounding, List()) //encontrar todas as posicoes q sao o opponent a volta do opponent atual (x)
            val libertiesFound= getLiberties(surrounding, List()) //nas pecas iguais (opponent) marca as liberdades

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
            //assim adiciouna espacos vazios e espacos com o proprio jogador para verificar se foi capturada
          }
          else{ //caso contrario continuar a percurrer
            getLiberties(xs, currentLiberties)
          }
        }
      }
    }

    @tailrec //junta as listas dadas de forma a nao ter valores repetidos
    def joinLists(newL: List[Coord2D], currentL: List[Coord2D]): List[Coord2D] = {
      newL match{
        case Nil => currentL
        case x::xs => {
          if(currentL.contains(x)){ //se ja tem o valor
            joinLists(xs, currentL) //passa valor e continuar a percurrer
          }
          else{ //se nao tem o valor adiciona a coordenada
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

    @tailrec //tira as coordenadas (pecas) dadas do ecra
    def atualizarBoard(currentBoard: Board, coordenadas: List[Coord2D]): Board = {
      coordenadas match{
        case Nil => currentBoard
        case x::xs => { //remove uma coordenada
          val linha= x._1
          val coluna= x._2

          def removeStone(remainingBoard: Board, currentLine: Int): Board = {
            remainingBoard match { //vai a procura da linha
              case Nil => Nil
              case y :: ys =>
                if (currentLine == linha) {
                  val newLine = alterLine(y, 0) //index da coluna comeca sempre em 0
                  newLine :: ys
                }
                else {
                  y :: removeStone(ys, currentLine+1)
                }
            }
          }

          def alterLine(line: List[Stone], currentCol: Int): List[Stone] = {
            line match { //procura a coluna
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

          //removeStone tira a peca na posicao x, initializando a currentLine a 0
          //chama a fn recursivamente para remover o resto das coordenadas (xs)
          atualizarBoard(removeStone(currentBoard, 0), xs)
        }
      }
    }

    //verifica cada grupo de pecas do adversario para ver se foram capturados
    def checkGroups(possibleGroups: List[Coord2D], workingBoard: Board, currentCaptured: Int): (Board, Int) = {
      possibleGroups match{
        case Nil => (workingBoard, currentCaptured) //se nao encontrou nenhum grupo capturado
        case x::xs => { //x seria uma peca do opponent
          val (liberties, stones)= findGroup(List(x), List(), List()) //encontra o grupo e as suas liberties
          val captured= checkLiberties(liberties)

          if(captured){ //se o grupo foi capturado
            val novoBoard= atualizarBoard(workingBoard, stones)
            val total= stones.size //numero de pecas capturadas e o numero de pecas no grupo

            checkGroups(xs, novoBoard, total+currentCaptured) //verificar proximo grupo com o tabuleiro atualizado
          }
          else{ //se nao capturou as pecas, continua com os valores initiais
            checkGroups(xs, workingBoard, currentCaptured)
          }
        }
      }
    }

    //codigo principal
    val surrounding= getSurroundingStones(playerCoord) //encontra pecas a volta do jogador
    val opponents= findOpponent(surrounding, List()) //encontra as pecas do opponent a volta do jogador
    checkGroups(opponents, board, 0) //faz captura de grupos (q estao capturados)
  }

  //T6
  //verifica se as pecas brancas ou as pretas ganharam o jogo (se já foram capturadas o número de peças necessárias)
  def seGanhou(): Option[Stone] = { //devolve a pedra q ganhou, ou None se ninguem ganhou
    val toWin= gameStateAtual.toWin
    val captureW= gameStateAtual.captureWhite
    val captureB= gameStateAtual.captureBlack

    if(captureW == toWin){ //se branco ganhou
      return Some(Stone.White)
    }

    if(captureB == toWin){ //se o preto ganhou
      return Some(Stone.Black)
    }

    //se ninguem ganhou
    None
  }

  //T7
  def timer(): Thread = {
    val t = new Thread() {
      override //override para fazer o nosso run em fez do default
      def run(): Unit = {
        val timeToPlay = tempoDeJogada * 1000 //tempo em millisegundos

        try{
          Thread.sleep(timeToPlay) //o thread espera o tempo dado

          //se ja passaram os 15 segundos (fez timeout) muda para o proximo jogador
          if(gameStateAtual.currentPlayer == Stone.White){
            gameStateAtual= GameState(gameStateAtual.toWin, gameStateAtual.captureWhite, gameStateAtual.captureBlack, gameStateAtual.board, gameStateAtual.freeCoord, Stone.Black)
            println("Agora é a vez do Black")
            currentThread= timer() //comeca novo timer para proxima jogada
          }
          else{
            gameStateAtual= GameState(gameStateAtual.toWin, gameStateAtual.captureWhite, gameStateAtual.captureBlack, gameStateAtual.board, gameStateAtual.freeCoord, Stone.White)
            println("Agora é a vez do White")
            currentThread= timer()
          }
        }
        catch{
          case i: InterruptedException => {
            println("interrompido!") //interrompido quando uma peca e jogada antes do tempo acabar
          }
        }
      }
    }

    t.start() //comeca a fazer run
    t //devolve timer para poder interromper fora da funcao
  }

  def undo(): Option[GameState] = {
    jogadas match {
      case Nil => None //se nao da para fazer undo mais (ja estamos no estado initial)
      case anterior :: restantes =>
        jogadas = restantes //atualiza a lista de jogadas para excluir a ultima
        gameStateAtual= restantes.head //atualiza o gameStateAtual para o antes do antingo atual
        Some(anterior)
    }
  }

  //T8
  //devolve um tabuleiro vazio e as suas coordenadas livres
  def newBoard(coluna:Int, tempBoard:Board, tempList:List[Coord2D], linha:Int): (Board,List[Coord2D]) = {
    if(linha > 0){
      newBoard(coluna, createLine(coluna, Nil)::tempBoard, createCoor(coluna,tempList,linha), linha-1)
    }
    else{
      (tempBoard,tempList)
    }
  }

  def createLine(a:Int, temp: List[Stone]): List[Stone] = {
    if(a > 0){
      createLine(a-1, Stone.Empty :: temp )
    }
    else {
      temp
    }
  }

  def createCoor(coluna: Int, temp:List[Coord2D], linha:Int): List[Coord2D]  = {
    if( coluna > 0){
      createCoor(coluna-1, (linha-1,coluna-1)::temp, linha)
    }
    else {
      temp
    }
  }

  //cria um novo gamestate, e atualiza as attributes gameStateAtual e jogadas
  def novaJogada(toWin: Int, captureWhite: Int, captureBlack: Int, board: Board, livres: List[Coord2D], nextPlayer: Stone): Unit = {
    val estadoAtual = GameState(toWin, captureWhite, captureBlack, board, livres, nextPlayer)
    jogadas = estadoAtual :: jogadas // Adiciona à head da lista (mais recente primeiro)
    gameStateAtual= estadoAtual
  }

  //funcao para completar uma jogada
  def turno(coordJogada: Coord2D): Unit = {
    val tabuleiro = gameStateAtual.board
    val coordLivres = gameStateAtual.freeCoord
    val player= gameStateAtual.currentPlayer

    val (novoTabuleiro, novasLivres) = play(tabuleiro, player, coordJogada, coordLivres) //faz uma jogada
    val optBoard = novoTabuleiro.getOrElse(None)
    if(optBoard != None){ //se jogou numa posicao valida
      currentThread.interrupt() //cancela o timer antigo para nao fazer time out
      val board = novoTabuleiro.get
      val (capturedBoard, captured)= getGroupStones(board, player, coordJogada) //captura pecas depois de jogada

      if(player == Stone.White){
        //atualiza o numero de pecas q o branco capturou (pode ser 0) e atualiza o proximo jogador (preto)
        novaJogada(gameStateAtual.toWin, gameStateAtual.captureWhite+captured, gameStateAtual.captureBlack, capturedBoard, novasLivres, Stone.Black)

        val optStone = seGanhou()
        val ganhou = optStone.getOrElse(None)
        if(ganhou != None){ //se alguem ganhou acaba jogo
          winner= optStone.get
        }
        else{ //se ninguem ganho
          currentThread= timer() //recomeca um novo timer para o proximo turno
        }
      }
      else{ //se for o jogador preto
        //atualiza o numero de pecas q o branco capturou (pode ser 0) e atualiza o proximo jogador (preto)
        novaJogada(gameStateAtual.toWin, gameStateAtual.captureWhite, gameStateAtual.captureBlack+captured, capturedBoard, novasLivres, Stone.White)

        val optStone = seGanhou()
        val ganhou = optStone.getOrElse(None)
        if(ganhou != None){ //se acaba jogo
          winner= optStone.get
        }
        else{ //se ninguem ganho
          currentThread= timer() //recomeca um novo timer para o proximo turno
        }
      }
    }
    else{ //se nao jogou numa posicao valida nao faz nada
      println("Jogada não válida!")
    }
  }

  //funcao para completar uma jogada aleatorio
  def turnoRandom(): Coord2D = {
    currentThread.interrupt() //cancela o timer antigo para nao fazer time out

    val tabuleiro = gameStateAtual.board
    val coordLivres = gameStateAtual.freeCoord
    val player= gameStateAtual.currentPlayer

    val rand = new MyRandom(System.currentTimeMillis())
    println("Hi")
    val (novoTabuleiro, _, novasLivres) = playRandomly(tabuleiro, rand, player, coordLivres, randomMove) //joga sempre numa posicao valida
    val (coordJogada, _)= randomMove(coordLivres, rand) //mesmo seed entao devolve o mesmo valor

    val (capturedBoard, captured)= getGroupStones(novoTabuleiro, player, coordJogada) //captura pecas

    println("captured:")
    printBoard(capturedBoard)

    if(player == Stone.White){
      novaJogada(gameStateAtual.toWin, gameStateAtual.captureWhite+captured, gameStateAtual.captureBlack, capturedBoard, novasLivres, Stone.Black)

      val optStone = seGanhou()
      val ganhou = optStone.getOrElse(None)
      if(ganhou != None){ //acaba jogo
        winner= optStone.get
        coordJogada //devolve coordenada jogada para o textflow
      }
      else{ //se ninguem ganho
        currentThread= timer() //recomeca um novo timer para o proximo turno
        coordJogada
      }
    }
    else{ //se for o jogador preto
      novaJogada(gameStateAtual.toWin, gameStateAtual.captureWhite, gameStateAtual.captureBlack+captured, capturedBoard, novasLivres, Stone.White)

      val optStone = seGanhou()
      val ganhou = optStone.getOrElse(None)
      if(ganhou != None){ //acaba jogo
        winner= optStone.get
        coordJogada
      }
      else{ //se ninguem ganho
        currentThread= timer() //recomeca um novo timer para o proximo turno
        coordJogada
      }
    }
  }

  //comeca um jogo novo
  def startGame() = {
    val (blankBoard, freeCoords) = newBoard(5,Nil,Nil,5) //gera novo tabuleiro e coordenadas livres
    gameStateAtual = GameState(numeroDeCapturas,0,0,blankBoard, freeCoords, Stone.Black) //gameStateInitial
    jogadas = List(gameStateAtual) //initializar gameState
    currentThread.interrupt() //vai parrar o thread atual para poder comecar um timer()
    currentThread = timer()
  }

  //Testes
  def main(args: Array[String]): Unit = {
    print("Peças a capturar para ganhar: ")
    val numeroDeCapturas= scala.io.StdIn.readInt() //pede numero de capturas
    println("")

    GameEngine.startGame()
    GameEngine.printBoard(GameEngine.gameStateAtual.board)
    while(GameEngine.winner == Stone.Empty){
      print("Quer fazer uma jogada aleatoria? (true/false): ")
      val playRandom= scala.io.StdIn.readBoolean()
      println("")

      if(playRandom){
        val rand= MyRandom(System.currentTimeMillis())
        GameEngine.turnoRandom()
        GameEngine.printBoard(GameEngine.gameStateAtual.board)
      }
      else{
        print("Row index (começa a contar por 0): ")
        val coordX= scala.io.StdIn.readInt()
        println("")
        print("Column index (começa a contar por 0): ")
        val coordY= scala.io.StdIn.readInt()
        println("")
        val coord= (coordX, coordY)

        GameEngine.turno(coord)
        GameEngine.printBoard(GameEngine.gameStateAtual.board)
      }
    }

    if(GameEngine.winner == Stone.Black){
      println("Black is the winner!")
    }
    else{ //se for o jogador branco q ganhou
      println("White is the winner!")
    }
  }
}
