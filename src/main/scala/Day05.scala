package AOC2022
import scala.io.Source
import scala.collection.mutable

object Day05 {
  type MyBox = List[(BoxStack, Int)]
  case class BoxStack(boxes: List[Char])

  val instructions = Source.fromResource("Day05.txt").getLines.map {
    case (s"move ${box} from ${from} to ${to}") => ((box.toInt, from.toInt, to.toInt))
  }.toList

  val box1 = BoxStack(List('J', 'H', 'G', 'M', 'Z', 'N', 'T', 'F'))
  val box2 = BoxStack(List('V', 'W', 'J'))
  val box3 = BoxStack(List('G', 'V', 'L', 'J', 'B', 'T', 'H'))
  val box4 = BoxStack(List('B', 'P', 'J', 'N', 'C', 'D', 'V', 'L'))
  val box5 = BoxStack(List('F', 'W', 'S', 'M', 'P', 'R', 'G'))
  val box6 = BoxStack(List('G', 'H', 'C', 'F', 'B', 'N', 'V', 'M'))
  val box7 = BoxStack(List('D', 'H', 'G', 'M', 'R'))
  val box8 = BoxStack(List('H', 'N', 'M', 'V', 'Z', 'D'))
  val box9 = BoxStack(List('G', 'N', 'F', 'H'))

  val boxes = List(box1, box2, box3, box4, box5, box6, box7, box8, box9).zipWithIndex

  def moveBox(times: Int, from: Int, to: Int, boxes: MyBox) : MyBox =
    val copyOfOldBoxes = boxes.find(((_, idx) => idx == from)).get._1.boxes
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

  def Day05Part1 =
    def repeatMoving(moves: Int)(from: Int, to: Int, input: MyBox) : MyBox =
      if (n > 0) {
        val newBoxes = moveBox(1, from, to, input)
        repeatMoving(n - 1)(from, to, newBoxes)
      }
      else
        input

    val newStackOfBoxes = instructions.foldLeft(boxes) {
      case ((mybox), (times, from, to)) => repeatMoving(times)(from - 1, to -1, mybox)
    }
    val answer = newStackOfBoxes.map((boxes, idx) => boxes.boxes.last).mkString

    println(s"Day 5 - part 1: ${answer}")

  def Day05Part2 =
    val newStackOfBoxes = instructions.foldLeft(boxes) {
      case ((mybox), (times, from, to)) => moveBox(times, from - 1, to -1, mybox)
    }
    val answer = newStackOfBoxes.map((boxes, idx) => boxes.boxes.last).mkString

    println(s"Day 5 - part 2: ${answer}")

  def main(args: Array[String]): Unit =
    Day05Part1
    Day05Part2
}
