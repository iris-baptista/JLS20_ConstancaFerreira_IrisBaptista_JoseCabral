import GameEngine.{Board, Coord2D} //assim para poder adicionar mais coisas se precisar
import Stone.Stone

case class GameState(toWin: Int, captureWhite: Int, captureBlack: Int, board: Board, freeCoord: List[Coord2D], currentPlayer: Stone)
//guarda o numero de pecas q tem de ser capturadas para ganhar,
//o numero de pecas q o jogador branco ja capturou,
//o numero de pecas q o jogador preto capturou,
//o tabuleiro atual do jogo
//as coordenadas livres do jogo atual
//o jogador do turno atual