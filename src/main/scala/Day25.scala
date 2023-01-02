package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day25 {
  val input = Source.fromResource("Day25.txt").getLines.toList

  def converter(line: String): Long =
    line.foldLeft(0L) { (sum, digit) =>
      val add = digit match
        case '0' => 0
        case '1' => 1
        case '2' => 2
        case '=' => -2
        case '-' => -1
      ((5 * sum) + add)
    }

  def toSnafu(n: Long, digits: ArrayBuffer[String]): String =
    val calculated = ArrayBuffer[String]()
    var number = n

    while (number > 0)
      val digit = number % 5

      val conv = digit match
        case 0 => "0"
        case 1 => "1"
        case 2 => "2"
        case 3 => "="
        case 4 => "-"

      val mappedDigit = digit match
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case 3 => -2
        case 4 => -1

      calculated.prepend(conv)
      number = ((number - mappedDigit) / 5)

    calculated.mkString

  def Day25Part1 =
    val answer = input.map(x => converter(x)).sum
    val converted = toSnafu(answer, ArrayBuffer.empty)

    println(s"Day 25 - part 1: ${converted}")

  // def main(args: Array[String]): Unit =
  //   Day25Part1
}