import FloydWarshall.{Array4D, ShortestPath}
import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import scala.annotation.tailrec
import scala.language.postfixOps
import Transitions.Transition

case class PathView(trace: List[Position], total: Int) {

  def covers(board: Board): Boolean =
    board.points == trace.toSet

  def consistsOf(moves: Set[Move]): Boolean = {
    trace.sliding(2).forall {
      case List(Position(ax,ay), Position(bx,by)) => moves.contains {
        Move(bx - ax, by - ay)
      }
    }
  }

}

case class Traverse(moves: Set[Move], board: Board, start: Position) {
  private val log = Logger[Traverse]

  private val initial: Path = Path(board, start)

  private val transitions: Set[Transition] = Transitions(moves, board)

  private val paths: Array4D[Option[ShortestPath]] = FloydWarshall
    .findShortestPathsFast(transitions)(board.width, board.height)

  import Parameters._

  def calculate(): PathView =
    calculateMain().view

  private def calculateMain(): Path = {
    // It is possible to just use `advance(paths)` method which walks over board
    // using distance matrix. It is the fastest way, but it generates pretty long paths.
    // So, I mix it with some in-breadth search on every move and sort all candidates with priority queue.
    // This reduces path length to about 30 points for 10x10.

    val queue = mutable.PriorityQueue[Path](initial)

    var result: Option[Path] = None
    while (result.isEmpty) {
      val passed = queue.dequeueAll.take(AmountToKeep)

      val selected = passed.headOption
      result = selected.find(path => path.covers(board))

      if (result.isEmpty) {
        val path = selected.get
        if (path.left >= BreadthFirstThreshold) {
          queue.enqueue(passed.drop(1): _*)
        } else {
          val almost = passed.drop(1).filter(_.left < BreadthFirstThreshold)
          queue.enqueue(almost: _*)
        }

        val generated = selected.toSet.flatMap { path: Path =>
          val breadthFirstCandidates = if (selected.get.left >= BreadthFirstThreshold) {
            transitions.flatMap(transition => path.advance(transition))
          } else Set()

          val floydWarshallCandidates = path.advance(paths)

          breadthFirstCandidates ++ floydWarshallCandidates
        }

        queue.enqueue(generated.toSeq: _*)
      }
    }

    result.get
  }

  def calculateFast(): PathView = {
    // Some trick for increasing speed
    if (board.width == 10 && board.height == 10) {
      val small = Board(5,5)

      val nearest = Search.findNearestTarget(paths)(_, _)

      def separate(coordinate: Int): Int =
        if (coordinate < 5) 0 else 1

      def renumerateToSmall(position: Position, subsquare: (Int,Int)): Position =
        Position(position.x - 5 * subsquare._1, position.y - 5 * subsquare._2)

      def renumerateToBig(position: Position, subsquare: (Int,Int)): Position =
        Position(position.x + 5 * subsquare._1, position.y + 5 * subsquare._2)

      def unfold(square: (Int,Int)): Set[Position] =
        small.points.map { position =>
          Position(position.x + 5 * square._1, position.y + 5 * square._2)
        }

      val subsquares = mutable.Set((0,0), (1,0), (0,1), (1,1))

      var current = start
      var path = Path(board)
      while (subsquares.nonEmpty) {
        val subsquare = (separate(current.x), separate(current.y))
        assert(subsquares.contains(subsquare))
        subsquares -= subsquare

        val from = renumerateToSmall(current, subsquare)
        assert(small.contains(from))

        val smallPath = Traverse(moves, small, from).calculateMain().trace
          .map(renumerateToBig(_, subsquare))

        path = path.appendPath {
          val smallPath = Traverse(moves, small, from).calculateMain().trace
            .map(renumerateToBig(_, subsquare))

          assert(board.contains(smallPath.head))

          if (path.trace.isEmpty) smallPath
          else {
            assert(path.trace.head == smallPath.last)
            smallPath.init
          }
        }

        if (subsquares.nonEmpty) {
          val next = nearest(path.trace.head, subsquares.toSet.flatMap(unfold))
          path = path.appendPath(next.path)
          current = next.position
        }
      }

      path.view

    } else calculate()
  }
}

private case class Target(position: Position, path: List[Position], weight: Int)

private object Target {

  implicit val TargetOrd: Ordering[Target] = new Ordering[Target] {
    def compare(a: Target, b: Target): Int = a.weight.compare(b.weight)
  }

}

private case class Path(trace: List[Position], targets: Set[Position],
                        total: Int, repeated: Int, left: Int, weight: Double) {

  import Parameters._

  def view: PathView = PathView(trace.reverse, total)

  def covers(board: Board): Boolean =
    total - repeated == board.width * board.height &&
      view.covers(board)

  def advance(f: Position => Option[Position]): Option[Path] =
    f(trace.head).map(append)

  def advance(paths: Array4D[Option[ShortestPath]]): Seq[Path] = {
    if (targets.nonEmpty) {
      val candidates = Search.findBestTargets(paths)(AmountToGenerate, trace.head, targets)

      candidates.map { target =>
        appendPath(target.path)
      }
    } else Seq()
  }

  def appendPath(path: List[Position]): Path =
    path.foldRight(this) {
      case (position, result) => result.append(position)
    }

  private def append(point: Position): Path = {
    val traceContains = trace.contains(point)

    val alreadyVisited = if (traceContains) 1 else 0
    val isTarget = if (!traceContains && targets.contains(point)) 1 else 0

    Path(point +: trace, targets - point, total + 1, repeated + alreadyVisited, left - isTarget,
      Path.metric(left - isTarget, total + 1, repeated + alreadyVisited))
  }
}

private object Search {

  def findNearestTarget(paths: Array4D[Option[ShortestPath]])
                       (from: Position, targets: Set[Position]): Target =
    findTargets(paths, bestNotNearest = false)(1, from, targets).head

  def findBestTargets(paths: Array4D[Option[ShortestPath]])
                     (amount: Int, from: Position,
                      targets: Set[Position]): Seq[Target] =
    findTargets(paths, bestNotNearest = true)(amount, from, targets)

  private def findTargets(paths: Array4D[Option[ShortestPath]], bestNotNearest: Boolean)
                         (amount: Int, from: Position, targets: Set[Position]): Seq[Target] = {

    def walk(from: Position, to: Position): List[Position] = {
      @tailrec
      def collect(acc: List[Position], from: Position, to: Position): List[Position] = {
        if (from == to) from +: acc
        else {
          val path = paths(from.x)(from.y)(to.x)(to.y).get
          collect(from +: acc, path.next, to)
        }
      }

      paths(from.x)(from.y)(to.x)(to.y) match {
        case Some(path) => collect(List(), path.next, to)
        case None => List()
      }
    }

    val queue = mutable.PriorityQueue[Target]()
    for (to <- targets) {
      val path = walk(from, to)

      val weight = if (bestNotNearest) {
        path.toSet.intersect(targets).size
      } else {
        -1 * path.size
      }

      queue.enqueue(Target(to, path, weight))
    }

    queue.dequeueAll.take(amount)
  }

}

private object Path {

  import Parameters._

  implicit val PathOrd: Ordering[Path] = new Ordering[Path] {
    def compare(f: Path, g: Path): Int = f.weight.compare(g.weight)
  }

  def apply(board: Board): Path =
    Path(List(), board.points, 0, 0, board.points.size, 1)

  def apply(board: Board, start: Position): Path =
    apply(board).append(start)

  def metric(left: Int, total: Int, repeated: Int): Double =
    if (left >= BreadthFirstThreshold) {
      total / (1 + Math.pow(repeated, 2) + left)
    } else {
      (total - repeated) / (1 + left * left)
    }

}

object Parameters {

  val AmountToKeep = 512

  val AmountToGenerate = 4

  val BreadthFirstThreshold = 4

}