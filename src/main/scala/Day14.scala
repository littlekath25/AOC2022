package AOC2022
import scala.io.Source
import scala.collection.mutable._
import java.io._

object Day14 {
  case class Pos(y: Int, x: Int)
  val sandStart = Pos(0, 500)

  val input = Source.fromResource("Day14.txt").getLines.map(_.split(" -> ").toList.map{ value => val splitted = value.split(","); Pos(splitted(1).toInt, splitted(0).toInt)}).toList

  val calculateForGridMinMax = input.flatten ::: List(sandStart)

  val gridMinMaxY = (calculateForGridMinMax.sortBy(_.y).map(_.y).head, calculateForGridMinMax.sortBy(_.y).map(_.y).last)
  val gridMinMaxX = (calculateForGridMinMax.sortBy(_.x).map(_.x).head, calculateForGridMinMax.sortBy(_.x).map(_.x).last)

  def generateRockCoords(line: List[Pos]): List[Pos] =
    val putIntRocks = line.sliding(2).toList.flatMap { line =>
        val min = Pos(line(0).y.min(line(1).y), line(0).x.min(line(1).x))
        val max = Pos(line(0).y.max(line(1).y), line(0).x.max(line(1).x))
        val newCoords = (min.y to max.y).flatMap(y => (min.x to max.x).map(x => Pos(y, x))).toList
        newCoords
    }
    putIntRocks

  var listOfRocks = input.flatMap(line => generateRockCoords(line)).distinct
  var grid = scala.collection.mutable.ArraySeq.tabulate(1000, 1000)((y, x) => '.')
  putRocksIntoGrid(listOfRocks)

  def putRocksIntoGrid(listOfRocks: List[Pos]): List[Pos] = 
    listOfRocks match {
      case head :: next => 
        grid(head.y)(head.x) = '-'
        putRocksIntoGrid(next)
      case Nil => listOfRocks
    }

  def printToFile(name: String): Unit = 
    val answerFile = new PrintWriter(new File(s"src/main/resources/generated/$name"))
    
    grid.foreach { line =>
      line.foreach { value =>
        answerFile.write(value)
      }
      answerFile.write("\n")
    }
    answerFile.close
    
  def checkForExitPart1(possibleMoves: List[Pos]) =
    val outOfBounds = (possibleMoves.filter{ pos =>
      (pos.y < gridMinMaxY._1 || pos.y > gridMinMaxY._2 || pos.x < gridMinMaxX._1 || pos.x > gridMinMaxX._2)
    }.size > 0)
    outOfBounds

  def isRockOrSand(pos: Pos): Boolean =
    val rockOrSand = (grid(pos.y)(pos.x) == '-' || grid(pos.y)(pos.x) == '♥')
    rockOrSand

  def getPossibleMoves(toCheck: Pos) : List[Pos] =
    var moves =
      List(Pos(toCheck.y + 1, toCheck.x),
      Pos(toCheck.y + 1, toCheck.x - 1),
      Pos(toCheck.y + 1, toCheck.x + 1))
    if (toCheck.y + 1 >= gridMinMaxY._2 + 2)
      moves = List.empty
    moves

  def dropOneSand: Boolean =
    var current = sandStart
    var possibleMoves = getPossibleMoves(current)

    if (!possibleMoves.map(x => isRockOrSand(x)).contains(false))
      true
    else
      while (possibleMoves.map(x => isRockOrSand(x)).contains(false))
        if (!isRockOrSand(possibleMoves(0)))
          current = possibleMoves(0)
        else if (!isRockOrSand(possibleMoves(1)))
          current = possibleMoves(1)
        else if (!isRockOrSand(possibleMoves(2)))
          current = possibleMoves(2)

        possibleMoves = getPossibleMoves(current)
        // if (checkForExitPart1(possibleMoves))
        //   return true

      grid(current.y)(current.x) = '♥'
      false

  def fillCave(sand: Int): Int =
    if (dropOneSand == false)
      fillCave(sand + 1)
    else
      sand

  def Day14Part1 =
    println(s"ay 14 - part 1: ${fillCave(0)}")
    printToFile("Day14-01.txt")
    grid.map(row => row.map(ch => '.'))

  def Day14Part2 =
    println(s"Day 14 - part 2: ${fillCave(0)}")
    printToFile("Day14-02.txt")

  // def main(args: Array[String]): Unit =
    // Day14Part1
    // Day14Part2
}