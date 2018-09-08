import com.typesafe.scalalogging.Logger

object Solution extends App {
  val log = Logger[Solution.type]

  val ChessBoard = Board(10,10)

  val AllowedMoves = Moves.Circle

  if (args.length != 2) {
    log.error("Input position (x,y) of the pawn")
  } else {
    val position = Position(args(0).toInt, args(1).toInt)

    if (!ChessBoard.contains(position)) {
      log.error(s"Input position on board of size (${ChessBoard.width},${ChessBoard.height}).")
    } else {
      log.info(s"Board size: ${ChessBoard.width}x${ChessBoard.height}")
      log.info(s"Starting point: (${position.x},${position.y})")
      log.info(s"")

      {
        val start = System.nanoTime()

        log.info("[Calculating covering path {fast mode}]")
        val result = Traverse(AllowedMoves, ChessBoard, position).calculateFast()

        log.info(s"Found traversal consisting of ${result.trace.size} points")
        val points = result.trace.map(position => (position.x, position.y))
        log.info(s"${points.mkString(" -> ")}")

        val time = (System.nanoTime() - start) / 1000000
        log.info(s"[Calculated in ${time / 1000},${time % 1000} seconds]")
      }

      log.info("")

      {
        val start = System.nanoTime()

        log.info("[Calculating covering path {slow mode}]")
        val result = Traverse(AllowedMoves, ChessBoard, position).calculate()

        log.info(s"Found traversal consisting of ${result.trace.size} points")
        val points = result.trace.map(position => (position.x, position.y))
        log.info(s"${points.mkString(" -> ")}")

        val time = (System.nanoTime() - start) / 1000000
        log.info(s"[Calculated in ${time / 1000},${time % 1000} seconds]")
      }
    }
  }
}
