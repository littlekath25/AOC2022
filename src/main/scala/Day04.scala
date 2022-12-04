package AOC2022
import scala.io.Source

object Day04 {
  val input = Source.fromResource("Day04.txt").getLines.map { pairs => 
    val splitted = pairs.split(",")
    val one = splitted(0).split("-").map(_.toInt).toArray
    val two = splitted(1).split("-").map(_.toInt).toArray
    ((one(0) to one(1)), (two(0) to two(1)))
  }.toList

  def Day04Part1 =
    val answer = input.map { elves => 
      val overlappingTasks = elves._1.intersect(elves._2)
      (overlappingTasks == elves._1 || overlappingTasks == elves._2)
    }.filter(_ == true).size

    println(s"Day 4 - part 1: $answer")

  def Day04Part2 =
    val answer = input.map { elves =>
      elves._1.intersect(elves._2).nonEmpty
    }.filter(_ == true).size

    println(s"Day 4 - part 2: ${answer}")

  def main(args: Array[String]): Unit =
    Day04Part1
    Day04Part2
}
