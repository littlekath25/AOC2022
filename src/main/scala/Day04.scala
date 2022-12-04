package AOC2022
import scala.io.Source

object Day04 {
  val input = Source.fromResource("Day04.txt").getLines.map { x => 
    val splitted = x.split(",")
    val one = splitted(0).split("-").map(_.toInt).toArray
    val two = splitted(1).split("-").map(_.toInt).toArray
    ((one(0) to one(1)), (two(0) to two(1)))
  }.toList

  def Day04Part1 =
    val overlap = input.map { x => 
      val hallo = x._1.intersect(x._2)
      (hallo == x._1 || hallo == x._2)
    }

    val answer = overlap.filter(x => x == true).size

    println(s"Day 4 - part 1: $answer")

  def Day04Part2 =
    val overlap = input.map {
      x => 
      x._1.intersect(x._2).nonEmpty
    }

    val answer = overlap.filter(x => x == true).size

    println(s"Day 4 - part 2: ${answer}")

  def main(args: Array[String]): Unit =
    Day04Part1
    Day04Part2
}
