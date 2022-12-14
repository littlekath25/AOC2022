package AOC2022
import scala.io.Source

object Day01 {
  val input = Source.fromResource("Day01.txt").mkString
  val combinedList = input.split("\n\n").map(_.split("\n").map(_.toInt).toList.sum)

  def Day01Part1 =
    val answer = combinedList.max

    println(s"Day 01 - part 1: $answer")

  def Day01Part2 =
    val answer = combinedList.sorted.reverse.take(3).sum

    println(s"Day 01 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day01Part1
  //   Day01Part2
}
