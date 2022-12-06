package AOC2022
import scala.io.Source

object Day06 {
  val input = Source.fromResource("Day06.txt").getLines.flatMap(_.toList).toList

  def getFirstUniqueLetter(step: Int, signal: List[Char]): Int =
    input.sliding(step, 1).toList.map(_.distinct.size == step).zipWithIndex.filter((bool, _) => bool == true).head._2 + step

  def Day06Part1 =
    val answer = getFirstUniqueLetter(4, input)

    println(s"Day 06 - part 1: ${answer}")

  def Day06Part2 =
    val answer = getFirstUniqueLetter(14, input)

    println(s"Day 06 - part 2: ${answer}")

  def main(args: Array[String]): Unit =
    Day06Part1
    Day06Part2
}
