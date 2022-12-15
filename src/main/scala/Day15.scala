package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day15 {
  case class Pos(x: Long, y: Long)
  case class Interval(min: Long, max: Long)

  val input = Source.fromResource("Day15.txt").getLines.toArray.map { row => row match
    case (s"Sensor at x=$xS, y=$yS: closest beacon is at x=$xB, y=$yB") => 
      (Pos(xS.toLong, yS.toLong), Pos(xB.toLong, yB.toLong), (Math.abs(xB.toLong - xS.toLong) + Math.abs(yB.toLong - yS.toLong)))
  }

  def getTotalSizeOfInterval(interval: Interval): Long =
    interval.max - interval.min + 1

  def isOverlapping(current: Interval, other: Interval): Boolean =
    val totalSize = current.max.max(other.max) - current.min.min(other.min) 
    val sizeOfBothIntervals = (current.max - current.min) + (other.max - other.min)
    sizeOfBothIntervals >= totalSize

  def mergeTwoIntervals(current: Interval, other: Interval): Interval =
    Interval(current.min.min(other.min), current.max.max(other.max))

  def buildIntervalForRow(input: Array[(Pos, Pos, Long)], row: Int): Array[Interval] =
    val startInterval = Array.empty[Interval]
    input.foldLeft(startInterval) {
      case (intervals, (sensor, beacon, distance)) =>
        val fromOuterToRow = distance - (sensor.y - row).abs
        if (fromOuterToRow < 0)
          intervals
        else
          val measuredInterval = Interval(sensor.x - fromOuterToRow, sensor.x + fromOuterToRow)
          val (inIntervals, outIntervals) = intervals.partition(current => isOverlapping(current, measuredInterval))
          val newIntervals = inIntervals.foldLeft(measuredInterval) {
            case (current, next) => mergeTwoIntervals(current, next)
          } +: outIntervals
          newIntervals
    }

  def solve(input: Array[(Pos, Pos, Long)], row: Int): Long =
    val intervals = buildIntervalForRow(input, row)
    val sumOfEachInterval = intervals.map(interval => getTotalSizeOfInterval(interval)).sum
    val totalBeaconsOnTheRow = input.filter(pair => pair._2.y == row).map(row => row._2).distinct.size
    sumOfEachInterval - totalBeaconsOnTheRow

  def solve2(input: Array[(Pos, Pos, Long)], max: Int, row: Int): Pos =
    val intervals = buildIntervalForRow(input, row)
    if (intervals.size > 1 && row < max)
      val xOfDistressBeacon = intervals(0).max + 1
      Pos(xOfDistressBeacon, row)
    else
      solve2(input, max, row + 1)

  def calculateTuningFrequency(distressBeacon: Pos): Long = 
    (distressBeacon.x * 4000000) + distressBeacon.y

  def Day15Part1 =
    println(s"Day 15 - part 1: ${solve(input, 2000000)}")

  def Day15Part2 =
    println(s"Day 15 - part 2: ${calculateTuningFrequency(solve2(input, 4000000, 0))}")

  // def main(args: Array[String]): Unit =
  //   Day15Part1
  //   Day15Part2
}
