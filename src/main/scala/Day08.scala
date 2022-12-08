package AOC2022
import scala.io.Source

object Day08 {
  val max = 100
  val input1 = getPosition(Source.fromResource("Day08.txt").getLines.map(_.toList.map(_.asDigit)).toList)
  val input2 = input1.transpose
  val input3 = input1.map(row => 
    row.map { v =>
      (v._1, (v._2._1, v._2._2))
    }.reverse
  )
  val input4 = input2.map(row => 
    row.map { v =>
      (v._1, (v._2._1, v._2._2))
    }.reverse
  )

  def getPosition(input: List[List[Int]]): List[List[(Int, (Int, Int))]] =
    val positions = List.range(1, max).flatMap(x => List.range(1, max).map(y => (x, y)))
    (input.flatten zip positions).grouped(max - 1).toList

  def transformRow(rowTrees: List[(Int, (Int, Int))], newList: List[(Int, (Int, Int))], highest: Int): List[(Int, (Int, Int))] =
    rowTrees match {
      case head :: next =>
        if (head._1 > highest)
          transformRow(next, newList ::: List(head), head._1)
        else if (next == Nil)
          transformRow(next, newList ::: List(head), highest)
        else
          transformRow(next, newList, highest)
      case Nil => newList
    }

  def getHighestTree(input: List[List[(Int, (Int, Int))]]) : List[List[(Int, (Int, Int))]] = 
    input.map { row => transformRow(row, List.empty, -1) }

  def getScenicScore(tree: (Int, (Int, Int)), treesInFront: List[(Int, (Int, Int))], acc: Int): Int =
    treesInFront match {
      case head :: next if (head._1 >= tree._1) => 
          // println(s"COMPARE: ${head._1} WITH ${tree._1}")
          // println("YES GO OUT")
          acc + 1
      case head :: next =>
        getScenicScore(tree, next, acc + 1)
      case Nil => acc
    }

  def lookAround(forrest: List[(Int, (Int, Int))], tree: (Int, (Int, Int))): Int =
    val top = forrest.filter(treeList => treeList._2._1 < tree._2._1 && treeList._2._2 == tree._2._2).reverse
    val bottom = forrest.filter(treeList => treeList._2._1 > tree._2._1 && treeList._2._2 == tree._2._2)
    val left = forrest.filter(treeList => treeList._2._1 == tree._2._1 && treeList._2._2 < tree._2._2).reverse
    val right = forrest.filter(treeList => treeList._2._1 == tree._2._1 && treeList._2._2 > tree._2._2)
    val scoreTop = getScenicScore(tree, top, 0)
    val scoreBottom = getScenicScore(tree, bottom, 0)
    val scoreLeft = getScenicScore(tree, left, 0)
    val scoreRight = getScenicScore(tree, right, 0)
    // println(s"FOR THE TREE ${tree._1}(${tree._2})\nSCORE: $scoreTop + $scoreBottom + $scoreLeft + $scoreRight\n")
    scoreTop * scoreBottom * scoreLeft * scoreRight

  val trees = getHighestTree(input1).flatten ::: getHighestTree(input2).flatten ::: getHighestTree(input3).flatten ::: getHighestTree(input4).flatten
  
  def Day08Part1 =
    val answer = trees.map(_._2).distinct.size

    println(s"Day 08 - part 1: ${answer}")

  def Day08Part2 =
    val input2 = input1.flatten
    val input3 = input2
    val answer = input2.map(x => lookAround(input3, x)).max

    // println(s"Day 08 - part 2: ${lookAround(input2, (5, (4, 3)))}")
    println(s"Day 08 - part 2: ${answer}")

  def main(args: Array[String]): Unit =
    Day08Part1
    Day08Part2
}
