import scala.annotation.tailrec
import scala.util.Random
//stone devia ser um enumerado se clhar lol
object Stone extends Enumeration{
  type Board = List[List[Stone]]
  type Coord2D = (Int,Int)

  type Stone = Value
  val Black,White,Empty = Value



}
