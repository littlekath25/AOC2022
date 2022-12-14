package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day16 {
  case class Valve(flowRate: Int, valves: Vector[String])

  val start = "AA"
  val input = Source.fromResource("Day16.txt").getLines.map{ row => row match
    case (s"Valve $name has flow rate=$rate; $t $l to $v $valves") =>
      val splitted = valves.split(",").map(_.trim).toVector
      name -> Valve(rate.toInt, splitted)
    }.to(collection.mutable.Map)

  def distanceTable(node: String, graph: Map[String, Valve]): Map[(String, String), Int] =
    val shortestPath = Map[(String, String), Int]()

    graph.flatMap { valve =>
      valve._2.valves.map { to =>
        shortestPath += ((valve._1, to) -> 1)
      }
      shortestPath += ((valve._1, valve._1) -> 0)
    }

    graph.map { first =>
      graph.map { second =>
        if (!shortestPath.contains(first._1, second._1))
          second
        else
          graph.map { third =>
            if (!shortestPath.contains(first._1, third._1))
              third
            else
              if (shortestPath.contains(second._1, third._1))
                shortestPath((second._1, third._1)) =
                  shortestPath((second._1, third._1)).min(shortestPath((second._1, first._1)) + shortestPath((first._1, third._1)))
              else
                shortestPath((second._1, third._1)) =
                  shortestPath((second._1, first._1)) + shortestPath((first._1, third._1))
          }
      }
    }

    shortestPath

  def getPathsForValves(node: String, graph: Map[String, Valve]): Map[(String, String), Int] =
    val paths = graph.flatMap { valve =>
      distanceTable(valve._1, graph)
    }
    paths

  val distances = getPathsForValves(start, input)
  val withPressure = input.filter(x => x._2.flowRate > 0 || x._1 == start)

  val openValves: ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer.empty

  // def findPath(limit: Int, start: String = start, time: Int = 0): Int =
  //   if (withPressure.size != openValves.size)
  //     var totalPressure = 0
  //     withPressure.foreach { valve =>
  //       if (openValves.contains(valve._1) || !distances.contains((start, valve._1)))
  //         valve
  //       else
  //         val timeToOpen = time + distances((start, valve._1)) + 1
  //         if (timeToOpen <= limit)
  //           val pressure = valve._2.flowRate * (limit - timeToOpen)
  //           openValves += valve._1
  //           val recursivePressure = findPath(limit, valve._1, timeToOpen)
  //           totalPressure = totalPressure.max(pressure + recursivePressure)
  //     }
  //     totalPressure
  //   else
  //     0

  def findPathWithElephant(limit: Int, current: String = start, currentElephant: String = start, time: Int = 0, timeElephant: Int = 0): Int =
    if (withPressure.size != openValves.size)
      var totalPressure = 0
      withPressure.foreach { valve =>
        if (openValves.contains(valve._1) || !distances.contains((start, valve._1)))
          valve
        else
          var destination = valve._1
          val timeToOpen = time + distances((current, destination)) + 1
          val timeToOpenElephant = timeElephant + distances((currentElephant, destination)) + 1

          if (timeToOpen <= timeToOpenElephant && timeToOpen <= limit)
            val pressure = valve._2.flowRate * (limit - timeToOpen)
            openValves += destination
            val recursivePressure = findPathWithElephant(limit, destination, currentElephant, timeToOpen, timeElephant)
            openValves -= destination
            totalPressure = totalPressure.max(pressure + recursivePressure)

          else if (timeToOpenElephant <= timeToOpen && timeToOpenElephant <= limit)
            val pressure = valve._2.flowRate * (limit - timeToOpenElephant)
            openValves += destination
            val recursivePressure = findPathWithElephant(limit, current, destination, time, timeToOpenElephant)
            openValves -= destination
            totalPressure = totalPressure.max(pressure + recursivePressure)
      }
      totalPressure
    else
      0

  // def Day16Part1 =
  //   val answer = findPath(30)
  //   println(s"Day 16 - part 1: ${answer}")

  // def Day16Part2 =
  //   val total = findPathWithElephant(26)
  //   println(s"$total")

  // def main(args: Array[String]): Unit =
    // Day16Part1
    // Day16Part2
}
