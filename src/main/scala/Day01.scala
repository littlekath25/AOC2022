package AOC2023

import scala.io.Source

@main def Day01 =
  Day01Part1
  Day01Part2

val input = Source.fromResource("Example.txt").mkString

def Day01Part1 =
  val combinedList: List[List[String]] = input.split("\n\n").toList.map(_.split("\n").toList)
  val convertedList: List[Int] = combinedList.flatMap(_.map(_.toInt))
  val answer: Int = convertedList.max

  println(s"Day 1 - part 1: $answer")

def Day01Part2 =
  val combinedList: List[List[String]] = input.split("\n\n").toList.map(_.split("\n").toList)
  val convertedList: List[Int] = combinedList.flatMap(_.map(_.toInt)).sorted.reverse
  val answer: Int = convertedList.take(3).sum

  println(s"Day 1 - part 2: $answer")
