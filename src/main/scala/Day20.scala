package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day20 {
  case class Node(value: Long, var previous: Node, var next: Node)
  val input = Source.fromResource("Day20.txt").getLines.map(_.toLong).to(scala.collection.mutable.ArrayBuffer)

  def createCircularLinkedList(input: ArrayBuffer[Long], key: Long): ArrayBuffer[Node] =
    var converToNodes = input.map { number =>
      Node(number * key, null, null)
    }

    converToNodes.zipWithIndex.map { (node, idx) =>
      if (idx == 0)
        val nextIndex = idx + 1
        val previousIndex = converToNodes.size - 1
        node.previous = converToNodes(previousIndex)
        node.next = converToNodes(nextIndex)
      else if (idx == converToNodes.size - 1)
        val nextIndex = 0
        val previousIndex = idx - 1
        node.previous = converToNodes(((idx - 1) % converToNodes.size))
        node.next = converToNodes(0)
      else
        val nextIndex = idx + 1
        val previousIndex = idx - 1
        node.previous = converToNodes(previousIndex)
        node.next = converToNodes(nextIndex)
    }
    converToNodes

  def moveNodes(nodes: ArrayBuffer[Node], rounds: Int): ArrayBuffer[Node] =
    if (rounds > 0)
      for (node <- nodes) do
        val placesToMove = (node.value % (nodes.size - 1)).toInt
        val range = (0L until placesToMove.abs.toLong)

        if (placesToMove < 0L)
          for (i <- range)
            val nextNode = node.next
            val currentNode = node
            val previousNode = node.previous
            val previousPreviousNode = node.previous.previous
            
            nextNode.previous = previousNode
            previousNode.next = nextNode
            previousNode.previous = currentNode
            currentNode.next = previousNode
            currentNode.previous = previousPreviousNode
            previousPreviousNode.next = currentNode

        else
          for (i <- range)
            val nextNextNode = node.next.next
            val nextNode = node.next
            val currentNode = node
            val previousNode = node.previous
          
            nextNextNode.previous = currentNode
            currentNode.next = nextNextNode
            currentNode.previous = nextNode
            nextNode.next = currentNode
            nextNode.previous = previousNode
            previousNode.next = nextNode

      moveNodes(nodes, rounds - 1)
    else
      nodes

  def checkPosition(nodes: ArrayBuffer[Node], numberToCheck: Int): Long =
    val start = nodes.find(_.value == 0).get
    var current = start
    var max: Long = 0
    var range = Range(0, numberToCheck)

    for (i <- range)
      current = current.next
      max = current.value
    max

  def printNodes(toPrint: ArrayBuffer[Node]): Unit =
    toPrint.foreach { node =>
      println(s"${node.previous.value} ${node.value} ${node.next.value}")
    }

  def Day20Part1 =
    val parsed = createCircularLinkedList(input, 1)
    val movedNodes = moveNodes(parsed, 1)
    val (first, second, third) = (checkPosition(movedNodes, 1000), checkPosition(movedNodes, 2000), checkPosition(movedNodes, 3000))

    println(s"Day 20 - part 1: ${first + second + third}")

  def Day20Part2 =
    val parsed = createCircularLinkedList(input, 811589153L)
    val movedNodes = moveNodes(parsed, 10)
    val (first, second, third) = (checkPosition(movedNodes, 1000), checkPosition(movedNodes, 2000), checkPosition(movedNodes, 3000))

    println(s"Day 20 - part 2:  ${first + second + third}")

  // def main(args: Array[String]): Unit =
  //   Day20Part1
  //   Day20Part2
}