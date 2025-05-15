import GameEngine.{Board} //assim para poder adicionar mais coisas se precisar

case class GameState(toWin: Int, captureWhite: Int, captureBlack: Int, board: Board)

//tb devia ter coordenadas livres?