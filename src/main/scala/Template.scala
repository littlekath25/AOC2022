package AOC2022
import scala.io.Source

object Day00 {
  val input = Source.fromResource("Example.txt").mkString
  val combinedList = input.split("\n\n").map(_.split("\n").map(_.toInt).toList.sum)

  def Day00Part1 =
    val answer = combinedList.max

    println(s"Day 0 - part 1: $answer")

  def Day00Part2 =
    val answer = combinedList.sorted.reverse.take(3).sum

    println(s"Day 0 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day00Part1
  //   Day00Part2
}
