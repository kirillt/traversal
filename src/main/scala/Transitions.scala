import scala.language.postfixOps

case class Position(x: Int, y: Int)

case class Board(width: Int, height: Int) {

  def contains(position: Position): Boolean =
    0 <= position.x && position.x < width &&
      0 <= position.y && position.y < height

  def points: Set[Position] =
    (0 until width).flatMap { x =>
      (0 until height).map { y =>
        Position(x, y)
      }
    } toSet

}

case class Move(dx: Int, dy: Int) {

  def apply(board: Board)(position: Position): Option[Position] = {
    val result = Position(position.x + dx, position.y + dy)
    Some(result).filter(board.contains)
  }

}

object Moves {

  val Circle: Set[Move] =
    Set(Move(3,0), Move(0,3), Move(-3,0), Move(0,-3),
      Move(2,2), Move(2,-2), Move(-2,2), Move(-2,-2))


  val King: Set[Move] =
    Set(Move(1,0), Move(0,1), Move(-1,0), Move(0,-1),
      Move(1,1), Move(-1,1), Move(1,-1), Move(-1,-1))

  val Knight: Set[Move] =
    Set(Move(2,1), Move(1,2), Move(-1,2), Move(-2,1),
      Move(-2,-1), Move(-1,-2), Move(1,-2), Move(2,-1))

  val Bishop: Set[Move] =
    (1 to 7).flatMap { k =>
      Set(Move(k,k), Move(k,-k), Move(-k,k), Move(-k,-k))
    } toSet

  val Rook: Set[Move] =
    (1 to 7).flatMap { k =>
      Set(Move(k,0), Move(0,k), Move(0,-k), Move(-k,0))
    } toSet

  val Queen: Set[Move] =
      Bishop ++ Rook

  val WhitePawn: Set[Move] =
    Set(Move(0,1))

  val BlackPawn: Set[Move] =
    Set(Move(0,-1))

}

object Transitions {

  type Transition = Position => Option[Position]

  def apply(moves: Set[Move], board: Board): Set[Transition] =
    moves.map { move =>
      move.apply(board)(_)
    }

}