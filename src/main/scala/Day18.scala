package AOC2022
import scala.io.Source
import scala.collection.mutable.*

object Day18 {
  val input = Source.fromResource("Day18.txt").getLines.map { row =>
    row match {
      case (s"${first},${second},${third}") => (Pos(first.toInt, second.toInt, third.toInt) -> 6)
    }
  }.to(scala.collection.mutable.Map)

  object BB {
    def getBoudingBox: (Int, Int) =
      var start = (0, 0)
      val newMin = input.map { cube =>
        (cube._1 -> (cube._1.getMin, cube._1.getMax))
      }.toList.map(_._2)
      (newMin.minBy(_._1)._1, newMin.maxBy(_._2)._2)

    val boundingBox = (getBoudingBox._1 - 1, getBoudingBox._2 + 1)
  }

  val start = Pos(BB.boundingBox._1, BB.boundingBox._1, BB.boundingBox._1)

  case class Pos(x: Int, y: Int, z: Int) {    
    def getMin: Int =
      x.min(y.min(z))

    def getMax: Int =
      x.max(y.max(z))

    def plus(dx: Int, dy: Int, dz: Int): Pos = Pos(this.x + dx, this.y + dy, this.z + dz)

    def getNeighbors: List[Pos] =
      val deltas = List((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1))
      val neighbors = deltas.map { delta =>
        this.plus(delta._1, delta._2, delta._3)
      }.filter { position =>
        val range = (BB.boundingBox._1 to BB.boundingBox._2)
        (range.contains(position.x) && range.contains(position.y) && range.contains(position.z))
      }
      neighbors
  }

  def decrementFaces(cube: Pos) : Int =
    val neighbors = cube.getNeighbors
    neighbors.map { neighbor =>
      if (input.contains(neighbor))
        0
      else
        1
    }.sum

  def calculateTouchingCubes(input: Map[Pos, Int]): Int =
    input.map { cube =>
      decrementFaces(cube._1)
    }.sum


  def breathFirstSearch(start: Pos): ArrayBuffer[Pos] =
    val queue = scala.collection.mutable.Queue(start)
    var visited = scala.collection.mutable.ArrayBuffer[Pos](start)

    while (queue.size > 0)
      val current = queue.front
      queue.dequeue

      visited = visited.appended(current).distinct
      val neighbors = current.getNeighbors

      neighbors.foreach { neighbor =>
        if (!visited.contains(neighbor) && !queue.contains(neighbor) && !input.contains(neighbor))
          queue.enqueue(neighbor)
      }
    visited

  def getExterior(steamGrid: ArrayBuffer[Pos]): Int =
    var total = 0

    input.map { cube =>
      val neighbors = cube._1.getNeighbors
      neighbors.map { neighbor =>
        if (steamGrid.contains(neighbor))
          total = total + 1
      }
    }
    total

  // def Day18Part1 =
  //   val answer = calculateTouchingCubes(input)

  //   println(s"Day 18 - part 1: ${answer}")

  // def Day18Part2 =
  //   val steamGrid = breathFirstSearch(start)
  //   val answer = getExterior(steamGrid)

  //   println(s"Day 18 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day18Part1
  //   Day18Part2
}
