import Stone.{Black, Board, Coord2D, Empty, White}

object Board {
  val tamanho = 9 //imutavel :D
  def empty: Board = List.fill(tamanho)(List.fill(tamanho)(Empty)) // faz 9x9 vazio

  def apply(grid: List[List[Stone]]): Board = { // Construtor validando 9x9
    require(grid.length == tamanho && grid.forall(_.length == tamanho))
    grid
  }
}
