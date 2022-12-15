package AOC2022
import scala.io.Source
import scala.collection.mutable.Stack

object Day13 {
  val file = Source.fromResource("Day13.txt").getLines.toList
  var firstQueue = scala.collection.mutable.Queue[Char]()
  var secondQueue = scala.collection.mutable.Queue[Char]()

  def clearQueue =
    while (firstQueue.nonEmpty)
      firstQueue.dequeue
    while (secondQueue.nonEmpty)
      secondQueue.dequeue

  def queueItems(first: String, second: String) =
    clearQueue

    first.foreach { value =>
        if (value == '[' || value == ']' || value.isDigit || value == ':')
          firstQueue.enqueue(value)
      }

    second.foreach { value =>
      if (value == '[' || value == ']' || value.isDigit || value == ':')
        secondQueue.enqueue(value)
    }

  def dequeueBoth =
    firstQueue.dequeue
    secondQueue.dequeue

  def addListItemAfter(queue: scala.collection.mutable.Queue[Char]): scala.collection.mutable.Queue[Char] =
    val value = queue.front
    scala.collection.mutable.Queue[Char](queue.front, ']').enqueueAll(queue.tail)

  def solve(left: String, right: String): Boolean =
    var inOrder = false
    val first = left.replace("10", ":")
    val second = right.replace("10", ":")

    queueItems(first, second)

    while (firstQueue.nonEmpty && secondQueue.nonEmpty)

      if (firstQueue.front == secondQueue.front)
        dequeueBoth
      else if (firstQueue.front == ']' && secondQueue.front != ']')
        inOrder = true
        clearQueue
      else if (firstQueue.front != ']' && secondQueue.front == ']')
        clearQueue
      else if (firstQueue.front == '[' && secondQueue.front != '[')
        firstQueue.dequeue
        secondQueue = addListItemAfter(secondQueue)
      else if (firstQueue.front != '[' && secondQueue.front == '[')
        secondQueue.dequeue
        firstQueue = addListItemAfter(firstQueue)
      else if ((firstQueue.front.isDigit || firstQueue.front == ':') && (secondQueue.front.isDigit || secondQueue.front == ':'))
        if (firstQueue.front.toInt < secondQueue.front.toInt)
          inOrder = true
          clearQueue
        else if (firstQueue.front.toInt > secondQueue.front.toInt)
          clearQueue

    if (firstQueue.isEmpty && secondQueue.nonEmpty)
      inOrder = true
    inOrder

  def Day13Part1 =
    val input = file.filter(_.nonEmpty).grouped(2).toList.map(pair => (pair.head, pair.last))
    val calculatedList = input.map((first, second) => solve(first, second)).zipWithIndex
    val onlyTrue = calculatedList.filter(x => x._1 == true).map(_._2 + 1)

    println(s"Day 13 - part 1: ${onlyTrue.sum}")
    clearQueue

  def Day13Part2 =
    val newFile = file.filter(_.nonEmpty) ::: List("[[2]]", "[[6]]")
    val calculatedList = newFile.sortWith((first, second) => solve(first, second)).zipWithIndex

    val indexOf2 = calculatedList.find((str, idx) => str == "[[2]]").get._2 + 1
    val indexOf6 = calculatedList.find((str, idx) => str == "[[6]]").get._2 + 1

    println(s"Day 13 - part 2: ${indexOf2 * indexOf6}")

  // def main(args: Array[String]): Unit =
  //   Day13Part1
  //   Day13Part2
}
