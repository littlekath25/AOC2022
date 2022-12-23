package AOC2022

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.collection.mutable.*

object Day1702 {
  val rocks = Array (
    Rock(Array(Pos(0, 0), Pos(1, 0), Pos(2, 0), Pos(3, 0))),
    Rock(Array(Pos(1, 0), Pos(0, 1), Pos(1, 1), Pos(2, 1), Pos(1, 2))),
    Rock(Array(Pos(0, 0), Pos(1, 0), Pos(2, 0), Pos(2, 1), Pos(2, 2))),
    Rock(Array(Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(0, 3))),
    Rock(Array(Pos(0, 0), Pos(1, 0), Pos(0, 1), Pos(1, 1))))

  case class Pos(x: Int, y: Int)
  case class Rock(positions: Array[Pos]) {
    def move(to: Pos): Rock =
      val newPos = this.positions.map { pos =>
        Pos(pos.x + to.x, pos.y + to.y)
      }
      Rock(newPos)
    
    def canMove(grid: Array[Pos]): Boolean = 
      this.positions.forall{ pos => 
        pos.x > 0 && pos.x < 8 && !grid.contains(pos)
      }
  }

  case class State(instructions: Array[Char], grid: Array[Pos], rockIdx: Int, instrIdx: Int, totalHeight: Int):
    def drop: State =
      val currentRock = rocks(rockIdx % rocks.size)
      val firstMoveBeforeStart = currentRock.move(Pos(3, totalHeight + 4))
      val (nextPosition, nextinstrIdx) = moveAndDown(firstMoveBeforeStart, instrIdx)
      val newMaxHeight = totalHeight.max(nextPosition.positions.map(_.y).max)

      State(instructions, grid ++ nextPosition.positions, rockIdx + 1, nextinstrIdx, newMaxHeight)

    def moveAndDown(rock: Rock, instrIdx: Int): (Rock, Int) =
      val instruction = instructions(instrIdx % instructions.size)
      val moveToCheck =
        if (instruction == '>')
          rock.move(Pos(1, 0))
        else
          rock.move(Pos(-1, 0))

      val movedSideways = 
        if (moveToCheck.canMove(grid))
          moveToCheck
        else
          rock

      val moveDown = movedSideways.move(Pos(0, -1))

      if (moveDown.canMove(grid))
        moveAndDown(moveDown, instrIdx + 1)
      else
        (movedSideways, instrIdx + 1)

  def getHeightOfRounds(instructions: Array[Char], state: State, grid: Array[Pos], rounds: Int): Seq[Int] =
    val collectionOfHeights = ArrayBuffer[Int]()
    var newRounds = rounds
    var newState = state

    while (newRounds > 1)
      val afterFallState = newState.drop
      collectionOfHeights += afterFallState.totalHeight
      newState = afterFallState
      newRounds = newRounds - 1

    collectionOfHeights

  def calculateCycle(heightsOfAllRounds: Seq[Int], startGuess: Int, endGuess: Int): Long =
    val totalHeightOfOneCycle = heightsOfAllRounds(endGuess) - heightsOfAllRounds(startGuess)
    val cycleLength = endGuess - startGuess

    // FROM HERE ON THE CYCLE REPEATS
    val rocksLeft = 1000000000000L - (startGuess + 1)

    val totalAmountOfCycles = rocksLeft / cycleLength
    val incompleteCycleLength = (rocksLeft % cycleLength).toInt
    val totalRocksForCompleteCycles = (totalAmountOfCycles * totalHeightOfOneCycle)
    val totalRocksForIncompleteCycles = heightsOfAllRounds(startGuess + incompleteCycleLength)

    (totalRocksForCompleteCycles + totalRocksForIncompleteCycles)
    

  def runSimulation(instructions: Array[Char], state: State, grid: Array[Pos]): Long =
    val firstGuessOfCycle = 1000
    val roundsToGenerate = 3 * firstGuessOfCycle
    val heightsOfAllRounds = getHeightOfRounds(instructions, state, grid, roundsToGenerate)
    val deltaAllRounds = heightsOfAllRounds.sliding(2).map(pair => pair.last - pair.head).toArray

    val endGuess = deltaAllRounds.size - firstGuessOfCycle
    val pattern = deltaAllRounds.drop(deltaAllRounds.size - firstGuessOfCycle)
    val startGuess = deltaAllRounds.lastIndexOfSlice(pattern, endGuess - 1)

    calculateCycle(heightsOfAllRounds, startGuess, endGuess)

  def Day17Part2 =
    val instructions = io.Source.fromResource("Day17.txt").getLines.flatMap(_.toArray).toArray

    val initialGrid = Array.tabulate(8)(Pos(_, 0))
    val initialState = State(instructions, initialGrid, 0, 0, 0)

    val answer = runSimulation(instructions, initialState, initialGrid)
    
    println(s"Day 17 - part 1: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day17Part2
}
