package AOC2023

import scala.io.Source

@main def Day01 =
  Day01Part1
  Day01Part2

val input = Source.fromResource("Day01.txt").mkString

def Day01Part1 =
  val output = (for (s <- input.split("\n\n").toList) yield {
      s.split("\n").map(_.toInt).toList}
    ).filter(!_.isEmpty).map(_.sum).max

  println(s"Day 1 - part 1: $output")

def Day01Part2 =
  val output = (for (s <- input.split("\n\n").toList) yield {
      s.split("\n").map(_.toInt).toList}
    ).filter(!_.isEmpty).map(_.sum).sorted.reverse.take(3).sum

  println(s"Day 1 - part 2: ${output}")
