package AOC2022
import scala.io.Source

object Day05 {
  type MyBox = List[(BoxStack, Int)]
  case class BoxStack(boxes: List[Char])

  val input = 
    Source.fromResource("Day05.txt")
    .getLines
    .toList

  val instructions =
    input
    .dropWhile(!_.startsWith("m"))
    .map {
      case (s"move $box from $from to $to") => ((box.toInt, from.toInt, to.toInt))
    }.toList

  val boxes =
    input
    .takeWhile(!_.isEmpty)
    .toList
    .transpose
    .filter( row =>
      row.exists(_.isLetterOrDigit)
    ).map( row =>
      (BoxStack(row.filter(_ != ' ').slice(0, row.size - 1).reverse), row.last.asDigit)
    )

  def moveBox(times: Int, from: Int, to: Int, boxes: MyBox, reverse: Boolean) : MyBox =
    val copyOfOldBoxes = boxes(from - 1)._1.boxes
    var boxesToMove = copyOfOldBoxes.slice(copyOfOldBoxes.size - times, copyOfOldBoxes.size)

    boxes.map { currentBox =>
      if (currentBox._2 == from)
        val droppedBox = currentBox._1.boxes.slice(0, currentBox._1.boxes.size - times)
        (BoxStack(droppedBox), currentBox._2)
      else if (currentBox._2 == to)
        if (reverse)
          boxesToMove = boxesToMove.reverse
        val newStack = currentBox._1.boxes ::: boxesToMove
        (BoxStack(newStack), currentBox._2)
      else
        currentBox
    }

  def getAnswer(boxes: MyBox) : String =
    boxes.map((boxes, idx) => boxes.boxes.last).mkString

  def solve(reverse: Boolean) : String =
    val newStackBoxes = instructions.foldLeft(boxes) { case ((mybox), (times, from, to)) => moveBox(times, from, to, mybox, reverse) }
    getAnswer(newStackBoxes)

  def Day05Part1 =
    println(s"Day 05 - part 1: ${solve(true)}")

  def Day05Part2 =
    println(s"Day 05 - part 2: ${solve(false)}")

  // def main(args: Array[String]): Unit =
  //   Day05Part1
  //   Day05Part2
}
