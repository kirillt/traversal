import Transitions.Transition

import scala.collection.mutable

object FloydWarshall {

  case class ShortestPath(distance: Int, next: Position) {

    //it is allowed only to append an adjacent path
    def append(other: ShortestPath): ShortestPath =
      ShortestPath(distance + other.distance, next)

  }

  object ShortestPath {

    def min(f: ShortestPath, g: ShortestPath): ShortestPath =
      if (f.distance > g.distance) g else f

  }

  def findShortestPaths(transitions: Set[Transition])(width: Int, height: Int):
      Map[(Position, Position), ShortestPath] = {

    val positions = (0 until width).flatMap(i => (0 until height).map(j => Position(i,j)))

    val distances: mutable.Map[(Position,Position), ShortestPath] = mutable.Map()

    for (itself <- positions) {
      distances((itself,itself)) = ShortestPath(0, itself)
    }

    for (from <- positions) {
      for (to <- transitions.flatMap(_.apply(from))) {
        distances((from, to)) = ShortestPath(1, to)
      }
    }

    for (middle <- positions) {
      for (from <- positions) {
        for (to <- positions) {

          val throughMiddle = distances.get((from,middle)).flatMap { half1 =>
            distances.get((middle,to)).map { half2 =>
              half1.append(half2)
            }
          }

          throughMiddle.foreach { combined =>
            distances((from,to)) = distances.get((from,to))
              .map(original => ShortestPath.min(original, combined))
              .getOrElse(combined)
          }
        }
      }
    }

    distances.toMap
  }

  type Array4D[X] = Array[Array[Array[Array[X]]]]

  def findShortestPathsFast(transitions: Set[Transition])(width: Int, height: Int):
      Array4D[Option[ShortestPath]] = {

    val positions = (0 until width).flatMap { x =>
      (0 until height).map(y => Position(x,y))
    }

    val distances: Array4D[Option[ShortestPath]] = Array.ofDim(width, height, width, height)

    for (from <- positions) {
      for (to <- positions) {
          distances(from.x)(from.y)(to.x)(to.y) = if (from == to) {
              Some(ShortestPath(0, from))
          } else None
        }

      transitions.flatMap(_.apply(from)).foreach { neighbour =>
        distances(from.x)(from.y)(neighbour.x)(neighbour.y) = Some(ShortestPath(1, neighbour))
      }
    }

    for (middle <- positions) {
      for (from <- positions) {
        for (to <- positions) {

          val throughMiddle = distances(from.x)(from.y)(middle.x)(middle.y).flatMap { half1 =>
            distances(middle.x)(middle.y)(to.x)(to.y).map { half2 =>
              half1.append(half2)
            }
          }

          throughMiddle.foreach { combined =>
            distances(from.x)(from.y)(to.x)(to.y) = distances(from.x)(from.y)(to.x)(to.y)
              .map(original => ShortestPath.min(original, combined))
              .orElse(throughMiddle)
          }
        }
      }
    }

    distances
  }

}
