package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day12 {
  case class Pos(y: Int, x: Int)
  var input = Source.fromResource("Day12.txt").getLines.map(_.toArray).toArray
  val coords = (0 to 40).flatMap ( y => (0 to 112).map (x => (y, x))).toArray

  val start = input.map { row =>
    if (row.contains(83))
      Pos(input.indexOf(row), row.indexOf(83))
    else
      Pos(0, 0)
  }.filter(pos => !(pos.x == 0 && pos.y == 0)).head

  val dest = input.map { row =>
    if (row.contains(69))
      Pos(input.indexOf(row), row.indexOf(69))
    else
      Pos(0, 0)
  }.filter(pos => !(pos.x == 0 && pos.y == 0)).head

  var gridWithHeight = input.map{_.map {x => if (x == 'E') 122 else if (x == 'S') 97 else x.toInt}}.toArray
  val visited = gridWithHeight.map(line => line.map(_ => false))
  val shortestPath = gridWithHeight.map(line => line.map(x => 1000))
  shortestPath(dest.y)(dest.x) = 0
  var queue = scala.collection.mutable.Queue(dest)

  def checkIfOutOfBounds(pos: Pos): Boolean =
    (pos.y >= 0 && pos.y < gridWithHeight.size) && (pos.x >= 0 && pos.x < gridWithHeight.head.size)

  def getShortestPath(queue: scala.collection.mutable.Queue[Pos], shortestPath: Array[Array[Int]], visited: Array[Array[Boolean]], size: Int) =
    while (queue.size > 0) {
      val current = queue.front
      queue.dequeue

      visited(current._1)(current._2) = true

      var neighbours = List(
        Pos(current.y, current.x - 1),
        Pos(current.y, current.x + 1),
        Pos(current.y - 1, current.x),
        Pos(current.y + 1, current.x)
      )

      neighbours = neighbours.filter(neighbour => checkIfOutOfBounds(neighbour))

      neighbours.foreach { neighbour =>
        val currentHeight = gridWithHeight(current.y)(current.x)
        val nextHeight = gridWithHeight(neighbour.y)(neighbour.x)

        if (currentHeight >= nextHeight - 1)
          val shortestDist = (shortestPath(neighbour.y)(neighbour.x)) + 1
          val currentShortestDist = shortestPath(current.y)(current.x)

          shortestPath(current.y)(current.x) = currentShortestDist.min(shortestDist)

        if (!visited(neighbour.y)(neighbour.x) && currentHeight <= nextHeight + 1)
          queue.enqueue(neighbour)
          visited(neighbour.y)(neighbour.x) = true
      }
    }

  def Day12Part1 =
    getShortestPath(queue, shortestPath, visited, queue.size)
    println(s"Day 12 - part 1: ${shortestPath(start.y)(start.x)}")

  def Day12Part2 =
    val zipped = input.flatten.zip(coords).zip(shortestPath.flatten).filter(value => value._1._1 == 'a' ||  value._1._1 == 'S').map(value => value._2).min

    println(s"Day 12 - part 2: ${zipped}")

  // def main(args: Array[String]): Unit =
  //   Day12Part1
  //   Day12Part2
}
