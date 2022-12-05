package AOC2022
import scala.io.Source

object Day05 {
  type MyBox = List[(BoxStack, Int)]
  case class BoxStack(boxes: List[Char])

  val instructions = 
    Source.fromResource("Day05.txt")
    .getLines.map {
      case (s"move $box from $from to $to") => ((box.toInt, from.toInt, to.toInt))
    }.toList

  val boxes = 
    Source.fromResource("Day05-Boxes.txt")
      .getLines
      .map(_.toList)
      .toList
      .transpose
      .filter( row =>
        row.exists(_.isLetterOrDigit)
      ).map( row =>
        (BoxStack(row.filter(_ != ' ').slice(0, row.size - 1).reverse), row.last.asDigit)
      )

  def moveBox(times: Int, from: Int, to: Int, boxes: MyBox) : MyBox =
    val copyOfOldBoxes = boxes(from - 1)._1.boxes
    val boxesToMove = copyOfOldBoxes.slice(copyOfOldBoxes.size - times, copyOfOldBoxes.size)
    boxes.map { currentBox =>
      if (currentBox._2 == from)
        val droppedBox = currentBox._1.boxes.slice(0, currentBox._1.boxes.size - times)
        (BoxStack(droppedBox), currentBox._2)
      else if (currentBox._2 == to)
        val newStack = currentBox._1.boxes ::: boxesToMove
        (BoxStack(newStack), currentBox._2)
      else
        currentBox
    }

  def repeatMoving(moves: Int)(from: Int, to: Int, input: MyBox) : MyBox =
    if (moves > 0)
      val newBoxes = moveBox(1, from, to, input)
      repeatMoving(moves - 1)(from, to, newBoxes)
    else
      input

  def getAnswer(boxes: MyBox) : String =
    boxes.map((boxes, idx) => boxes.boxes.last).mkString

  def Day05Part1 =
    val newStackBoxes = instructions.foldLeft(boxes) { case ((mybox), (times, from, to)) => repeatMoving(times)(from, to, mybox) }

    println(s"Day 5 - part 1: ${getAnswer(newStackBoxes)}")

  def Day05Part2 =
    val newStackBoxes = instructions.foldLeft(boxes) { case ((mybox), (times, from, to)) => moveBox(times, from, to, mybox) }

    println(s"Day 5 - part 2: ${getAnswer(newStackBoxes)}")

  def main(args: Array[String]): Unit =
    Day05Part1
    Day05Part2
}
