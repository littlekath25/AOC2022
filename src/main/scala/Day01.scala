package AOC2023

import scala.io.Source

@main def Day01 =
  Day01Part1
  Day01Part2

val input = Source.fromResource("Example.txt").mkString
val combinedList = input.split("\n\n").toList.map(_.split("\n").map(_.toInt).toList.sum)

def Day01Part1 =
  val answer = combinedList.max

  println(s"Day 1 - part 1: $answer")

def Day01Part2 =
  val answer = combinedList.sorted.reverse.take(3).sum

  println(s"Day 1 - part 2: ${answer}")
