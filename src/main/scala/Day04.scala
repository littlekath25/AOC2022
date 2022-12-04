package AOC2022
import scala.io.Source

object Day04 {
  val input = Source.fromResource("Day04.txt").getLines.map {
    case (s"${r1}-${r2},${r3}-${r4}") => ((r1.toInt to r2.toInt), (r3.toInt to r4.toInt))
  }.toList

  def Day04Part1 =
    val answer = input.count { elves => 
      val overlappingTasks = elves._1.intersect(elves._2)
      (overlappingTasks == elves._1 || overlappingTasks == elves._2)
    }

    println(s"Day 4 - part 1: $answer")

  def Day04Part2 =
    val answer = input.count { elves =>
      elves._1.intersect(elves._2).nonEmpty
    }

    println(s"Day 4 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day04Part1
  //   Day04Part2
}
