# Chessboard puzzle solver

The solver can be run as plain CLI program:
```
sbt "run 3 7"
```
This will run the solver for position (3,7) on board 10x10.

# Algorithm notes

The solver is implemented with use of Floyd-Warshall algorithm.
FW is enough for naive implementation, but it produces a bit long paths.
I added sort of heuristic search on top of FW for finding better paths,
but it started to work 5-10 times slower. That is why I also added
splitting 10x10 into 5x5 squares. Final version seeks paths in
isolated 5x5 squares, then looks for shortest path
to another square and continues search there.

# Implementation notes

Floyd-Warshall algorithm is implemented with 4-dimensional array and
separately with a bit more functinal style map. Array implementation
works significantly faster (5-8 times on 5x5 to 12x12 boards).
I used mutable collections as well in couple of places,
because it is algorithmical task after all. Also,
there is only mutable version
of PriorityQueue in Scala.

There are left some useful notes as comments in the code.
