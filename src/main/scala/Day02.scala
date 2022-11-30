package AOC2023

import scala.io.Source

@main def Day02 =
  Day02Part1
  Day02Part2

val input: List[(String, Int)] = Source.fromResource("Day02.txt").getLines.map(line => {
  val value = line.split(' ')
  (value(0), value(1).toInt)
}).toList

def Day02Part1 = 
  def position(instruction: (String, Int)) : (Int, Int) =
    instruction match {
      case (inst, number) if (inst.startsWith("forward")) => (number, 0)
      case (inst, number) if (inst.startsWith("up")) => (0, -number)
      case (inst, number) if (inst.startsWith("down")) => (0, number)
    }

  val answer = input.map(position).foldLeft((0,0)) { 
    case ((horizontal, depth), (instr, number)) => (horizontal + instr, depth + number) 
  }

  println(s"Answer of day 02 - part 1: ${answer._1 * answer._2}")

def Day02Part2 =
  val answer = input.foldLeft((0, 0, 0)) {
    case ((horizontal, depth, aim), (instr, number)) if (instr.startsWith("forward")) => (horizontal + number, depth + (aim * number), aim)
    case ((horizontal, depth, aim), (instr, number)) if (instr.startsWith("up")) => (horizontal, depth, aim - number)
    case ((horizontal, depth, aim), (instr, number)) if (instr.startsWith("down")) => (horizontal, depth, aim + number)
  }

  println(s"Answer of day 02 - part 2: ${answer._1 * answer._2}")
