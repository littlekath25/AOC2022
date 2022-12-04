package AOC2022
import scala.io.Source

object Day03 {
  val input = Source.fromResource("Day03.txt").getLines.toList

  def toPoints(item: Char) : Int =
    if (item.isLower)
      item.toInt - 96
    else
      item.toInt - 38

  def Day03Part1 =
    val groupedByItem = input.map { items => val splitted = items.splitAt(items.length / 2); (splitted._1, splitted._2) }
    val answer = groupedByItem.map(x => x._1.intersect(x._2).distinct).map(x => toPoints(x(0))).sum

    println(s"Day 3 - part 1: $answer")

  def Day03Part2 =
    val groupedByEleves = input.grouped(3)
    val answer = groupedByEleves.flatMap(x => x(0).intersect(x(1).intersect(x(2))).distinct.map(toPoints)).sum

    println(s"Day 3 - part 2: $answer")

  // def main(args: Array[String]): Unit =
  //   Day03Part1
  //   Day03Part2
}
