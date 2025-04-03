//import scala.annotation.tailrec
//import scala.util.Random
////stone devia ser um enumerado se clhar lol
//object Stone extends Enumeration{
//  type Board = List[List[Stone]]
//  type Coord2D = (Int,Int)
//
//  type Stone = Value
//  val Black,White,Empty = Value
//
//
//}

sealed trait Stone
object Stone {
  case object Black extends Stone
  case object White extends Stone
  case object Empty extends Stone

  type Board = List[List[Stone]]
  type Coord2D = (Int, Int)
}