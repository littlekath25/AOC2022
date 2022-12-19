package AOC2022
import AOC2022.Day14.grid

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.collection.mutable.*

object Day17 {
  val instructions = Source.fromResource("Example.txt").getLines.flatMap(_.toList).toList
  val grid = Array.tabulate(1, 7)((y, _) => if (y == 0) '_' else '.')

  val maxInstrNumber = instructions.size

  def printGrid(grid: Array[Array[Char]]): Unit =
    val answerFile = new PrintWriter(new File(s"src/main/resources/generated/Day17.txt"))
    grid.foreach { line =>
      answerFile.write("|")
      line.foreach { value =>
        answerFile.write(value)
      }
      answerFile.write("|")
      answerFile.write("\n")
    }
    answerFile.close

  def printGridInTerminal(grid: Array[Array[Char]]): Unit =
    grid.foreach {row => row.foreach {print}; println}

  case class Pos(y: Int, x: Int)

  case class Rock(positions: Array[Pos]) {
    def isOnBottom(grid: Array[Array[Char]]): Boolean =
      this.positions.map { pos =>
        if (grid(pos.y + 1)(pos.x) != '.' && !this.positions.contains(Pos(pos.y + 1, pos.x)))
          true
        else
          false
      }.exists(_ == true)

    def isBlockedLeft(grid: Array[Array[Char]]): Boolean =
      val minMax = this.positions.minBy(_.x)
      if (minMax.x == 0)
        true
      else
        this.positions.map { pos =>
          if (grid(pos.y)(pos.x - 1) != '.' && !this.positions.contains(Pos(pos.y, pos.x - 1)))
            true
          else
            false
        }.exists(_ == true)

    def isBlockedRight(grid: Array[Array[Char]]): Boolean =
      val minMax = this.positions.maxBy(_.x)
      if (minMax.x == 6)
        true
      else
        this.positions.map { pos =>
          if (grid(pos.y)(pos.x + 1) != '.' && !this.positions.contains(Pos(pos.y, pos.x + 1)))
            true
          else
            false
        }.exists(_ == true)

    def move(dir: Char, grid: Array[Array[Char]]): Rock =
      val newRock =
      dir match {
        case '>' =>
          if (this.isBlockedRight(grid))
            this
          else
            this.copy(positions = this.positions.map(pos => Pos(pos.y, pos.x + 1)))
        case '<' =>
          if (this.isBlockedLeft(grid))
            this
          else
            this.copy(positions = this.positions.map(pos => Pos(pos.y, pos.x - 1)))
      }
        newRock
    
    def down: Rock =
      val newRock = this.copy(positions = positions.map { stone =>
          stone.copy(y = stone.y + 1)
        })
      newRock

    def generateLines: Array[Array[Char]] =
      val maxHeight = this.positions.map(x => x.y).max + 1

      (0 until maxHeight).map { y =>
        (0 until 7).map { x =>
          if (this.positions.contains(Pos(y, x)))
            '♥'
          else
            '.'
        }.toArray
      }.toArray

    def printPositions: Unit =
      positions.foreach(x => print(s"$x "))
      println
    }

  val typeOfRocks = Array(
    Rock(Array(Pos(0, 2), Pos(0, 3), Pos(0, 4), Pos(0, 5))), // -
    Rock(Array(Pos(0, 3), Pos(1, 2), Pos(1, 3), Pos(1, 4), Pos(2, 3))), // +
    Rock(Array(Pos(0, 4), Pos(1, 4), Pos(2, 2), Pos(2, 3), Pos(2, 4))), // reverse L
    Rock(Array(Pos(0, 2), Pos(1, 2), Pos(2, 2), Pos(3, 2))), // I
    Rock(Array(Pos(0, 2), Pos(1, 2), Pos(0, 3), Pos(1, 3))) // square
  )

  def prependNewRock(grid: Array[Array[Char]], newRockLines: Array[Array[Char]]): Array[Array[Char]] =
    val clearedGRid = grid.dropWhile(row => !(row.contains('♥') || row.contains('_')))
    val gridWithEmptyRows = Array.tabulate(21)(x => '.').concat(clearedGRid.flatten).grouped(7).toArray
    newRockLines.flatten.concat(gridWithEmptyRows.flatten).grouped(7).toArray

  def moveRockUntillBottom(grid: Array[Array[Char]], rock: Int, instr: Int, maxRocks: Long):  Array[Array[Char]] =
    if (maxRocks > 0)
      val rockType = rock % 5
      var instrNumber = instr % maxInstrNumber

      val gridWithPrependedRock = prependNewRock(grid, typeOfRocks(rockType).generateLines)

      var currentRock = typeOfRocks(rockType)
      currentRock.positions.foreach { pos =>
        gridWithPrependedRock(pos.y)(pos.x) = '.'
      }
      
      currentRock = currentRock.move(instructions(instrNumber), gridWithPrependedRock)
      instrNumber = (instrNumber + 1) % maxInstrNumber
      
      while (!currentRock.isOnBottom(gridWithPrependedRock))
        currentRock.positions.foreach { pos =>
          gridWithPrependedRock(pos.y)(pos.x) = '.'
        }

        currentRock = currentRock.down
        currentRock = currentRock.move(instructions(instrNumber), gridWithPrependedRock)
        instrNumber = (instrNumber + 1) % maxInstrNumber
      
      currentRock.positions.foreach { pos =>
        gridWithPrependedRock(pos.y)(pos.x) = '♥'
      }
      moveRockUntillBottom(gridWithPrependedRock, rockType + 1, instrNumber, maxRocks - 1)
    else
      grid

  // def Day17Part1 =
  //   val newgrid = moveRockUntillBottom(grid, 0, 0, 2022)
  //   println(s"Day 17 - part 1: ${newgrid.count(row => row.contains('♥'))}")

  // def Day17Part2 =
  //   val newgrid = moveRockUntillBottom(grid, 0, 0, 1000000000000)
  //   println(s"Day 17 - part 2: ${newgrid.count(row => row.contains('♥'))}")

  // def main(args: Array[String]): Unit =
    // Day17Part1
    // Day17Part2
}
