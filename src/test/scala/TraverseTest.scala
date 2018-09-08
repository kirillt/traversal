import scala.language.postfixOps
import com.typesafe.scalalogging.Logger
import org.scalatest._

class TraverseTest extends FlatSpec {
  private val log = Logger[TraverseTest]

  private val Methods: Map[String, Traverse => PathView] = Seq(
    ("fast-search", (traverse: Traverse) => traverse.calculateFast()),
    ("short-path", (traverse: Traverse) => traverse.calculate())
  ) toMap

  "Paths" should "cover whole board and be formed by specified moves" in {
    for ((name,method) <- Methods) {
      log.info(s"Testing method `$name`:")
      for (board <- Seq(Board(5, 5), Board(10, 10))) {
        log.info(s"\tChecking board ${board.width}x${board.height}...")

        for (x <- 0 until board.width) {
          for (y <- 0 until board.height) {
            val path: PathView = method(Traverse(Moves.Circle, board, Position(x, y)))
            log.info(s"\t\tFound path of length ${path.total}")

            assert(path.covers(board))
            assert(path.consistsOf(Moves.Circle))
          }
        }
      }
    }
  }
}
