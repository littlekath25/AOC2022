package AOC2022
import scala.io.Source

object Day06 {
  val input = Source.fromResource("Day06.txt").getLines.flatMap(_.toList).toList

  def detectStartOfPacketMarker(rule: Int, datastream: List[Char])(f: List[Char] => Boolean): Int =
    datastream.sliding(rule, 1).toList.map(x => f(x)).zipWithIndex.filter((bool, _) => bool == true).head._2 + rule

  def Day06Part1 =
    val rule = 4
    val answer = detectStartOfPacketMarker(rule, input)(_.distinct.size == rule)

    println(s"Day 06 - part 1: ${answer}")

  def Day06Part2 =
    val rule = 14
    val answer = detectStartOfPacketMarker(rule, input)(_.distinct.size == rule)

    println(s"Day 06 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day06Part1
  //   Day06Part2
}
