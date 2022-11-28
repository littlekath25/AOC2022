package AOC2023

import scala.io.Source

@main def Day01 =
  Day01Part1
  Day01Part2

val input = Source.fromResource("Day01.txt").getLines.toList.map(_.toInt)

def Day01Part1 =
  val answer = input.sliding(2, 1).count(l => l(0) < l(1))
  println(s"Answer of day 01 - part 1: $answer")

def Day01Part2 =
  val answer = input.sliding(3, 1).map(_.sum).sliding(2).count(l => l(0) < l(1))
  println(s"Answer of day 01 - part 2: $answer")
