import FloydWarshall.ShortestPath
import com.typesafe.scalalogging.Logger
import org.scalatest.FlatSpec

class FloydWarshallTest extends FlatSpec {
  private val log = Logger[FloydWarshallTest]

  "Array implementation" should "satisfy basic properties" in {
    for (width <- Seq(5,10)) {
      for (height <- Seq(5,10)) {
        val search = FloydWarshall.findShortestPathsFast(Transitions(Moves.Circle, Board(width, height)))(_,_)
        val result = search(width, height)

        //there should be no negative numbers
        assert {
          result.forall(_.forall(_.forall(_.forall { path =>
            path.map(_.distance).getOrElse(0) >= 0
          })))
        }

        for (i <- 0 until width) {
          for (j <- 0 until height) {
            //path to itself is 0
            assert(result(i)(j)(i)(j).contains {
              FloydWarshall.ShortestPath(0, Position(i,j))
            })
          }
        }
      }
    }
  }

  "Map implementation" should "satisfy basic properties" in {
    for (width <- Seq(5,10)) {
      for (height <- Seq(5,10)) {
        val search = FloydWarshall.findShortestPaths(Transitions(Moves.Circle, Board(width, height)))(_,_)
        val result = search(width, height)

        //there should be no negative numbers
        assert(result.forall(_._2.distance >= 0))

        if (width == 5 && height == 5) {
          //Floyd-Warshall algorithm can't backtrack, it finds only paths leading to the target in one go;
          //and such paths exist between any positions only in "lucky" boards, like 5x5.
          assert(result.size == width * width * height * height)
        }

        for (i <- 0 until width) {
          for (j <- 0 until height) {
            //path to itself is 0
            assert(result((Position(i,j), Position(i,j))).distance == 0)
          }
        }
      }
    }
  }


  "Array and Map implementations" should "return same values" in {
    var (mapAvg, arrayAvg) = (0L, 0L)

    for (width <- 5 to 10) {
      for (height <- 5 to 10) {
        val mapSearch = FloydWarshall.findShortestPaths(Transitions(Moves.Circle, Board(width, height)))(_,_)
        val arraySearch = FloydWarshall.findShortestPathsFast(Transitions(Moves.Circle, Board(width, height)))(_,_)

        val (map, mapTime) = {
          val start = System.nanoTime()
          val result = mapSearch(width, height)
          (result, System.nanoTime() - start)
        }
        log.info(s"Time for `Map` on ${width}x$height:\t${mapTime}ns")
        mapAvg += mapTime

        val (array, arrayTime) = {
          val start = System.nanoTime()
          val result = arraySearch(width, height)
          (result, System.nanoTime() - start)
        }
        log.info(s"Time for `Array` on ${width}x$height:\t${arrayTime}ns")
        arrayAvg += arrayTime

        for (i <- 0 until width) {
          for (j <- 0 until height) {
            for (k <- 0 until width) {
              for (l <- 0 until height) {
                assert(map.get((Position(i,j), Position(k,l))) == array(i)(j)(k)(l))
              }
            }
          }
        }
      }
    }

    log.info(s"Array implementation looks to be faster in ${mapAvg.toDouble/arrayAvg} times")
  }

  "Floyd-Warshall algorithm" should "find correct distances for different chess pieces" in {
    val chessboard = Board(8,8)

    val king = Transitions(Moves.King, chessboard)
    val queen = Transitions(Moves.Queen, chessboard)
    val bishop = Transitions(Moves.Bishop, chessboard)
    val knight = Transitions(Moves.Knight, chessboard)
    val rook = Transitions(Moves.Rook, chessboard)
    val pawnWhite = Transitions(Moves.WhitePawn, chessboard)
    val pawnBlack = Transitions(Moves.BlackPawn, chessboard)

    def equalsTo(distance: Int, path: Option[ShortestPath]): Boolean =
      path.isDefined && path.forall(_.distance == distance)

    {
      val distances = FloydWarshall.findShortestPathsFast(king)(8, 8)
      assert(distances(0)(0)(7)(7).contains(ShortestPath(7, Position(1,1))))
      assert(equalsTo(3, distances(5)(7)(2)(7)))
    }

    {
      val distances = FloydWarshall.findShortestPathsFast(queen)(8, 8)
      assert(distances(0)(0)(7)(7).contains(ShortestPath(1, Position(7,7))))
      assert(distances(5)(7)(2)(7).contains(ShortestPath(1, Position(2,7))))
      assert(equalsTo(2, distances(4)(4)(5)(2)))
    }

    {
      val distances = FloydWarshall.findShortestPathsFast(bishop)(8, 8)
      assert(distances(0)(0)(0)(6).contains(ShortestPath(2, Position(3,3))))
      assert(equalsTo(2, distances(3)(3)(7)(3)))
      assert(distances(0)(0)(0)(1).isEmpty)
    }

    {
      val distances = FloydWarshall.findShortestPathsFast(knight)(8, 8)
      assert(distances(4)(4)(5)(2).contains(ShortestPath(1, Position(5,2))))
      assert(equalsTo(6, distances(0)(0)(7)(7)))
    }

    {
      val distances = FloydWarshall.findShortestPathsFast(rook)(8, 8)
      assert(distances(4)(4)(7)(4).contains(ShortestPath(1, Position(7,4))))
      assert(equalsTo(2, distances(4)(4)(5)(2)))
      assert(equalsTo(2, distances(0)(0)(7)(7)))
    }

    {
      val distances = FloydWarshall.findShortestPathsFast(pawnWhite)(8, 8)
      assert(distances(4)(4)(4)(7).contains(ShortestPath(3, Position(4,5))))
      assert(distances(0)(0)(1)(1).isEmpty)
      assert(distances(7)(7)(7)(6).isEmpty)
    }

    {
      val distances = FloydWarshall.findShortestPathsFast(pawnBlack)(8, 8)
      assert(distances(4)(4)(4)(0).contains(ShortestPath(4, Position(4,3))))
      assert(distances(1)(1)(0)(0).isEmpty)
      assert(distances(6)(6)(6)(7).isEmpty)
    }
  }

}
