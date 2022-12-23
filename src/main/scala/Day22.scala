package AOC2022
import scala.io.Source
import scala.collection.mutable._
import AOC2022.Day22_Utils._

object Day22 {
  val pattern = "([0-9]+)|([A-Z]+)".r
  val input = Source.fromResource("Day22.txt").getLines.map{ line => 
    line match
      case str =>
        if (str.forall(_.isLetterOrDigit))
          val instructions = pattern.findAllIn(str).toList
          instructions.map { singleInstr =>
            singleInstr match
              case i if (i.forall(_.isDigit)) => Steps(i.toInt)
              case "R"                        => R
              case "L"                        => L
          }.toList
        else
          str.mkString.zipWithIndex.toList
    }.toList

  val instructions: List[Instruction] = input.last.collect{ case x: Instruction => x }

  def getRangeForY(maze: List[List[(Char, Int, Int)]], currentRow: Int = 1): List[((Int, Int), Int)] = 
    val maxRow = maze.maxBy(_.size).size + 1
    if (currentRow == maxRow)
      Nil
    else
      val allYLengths = maze.map(row => row.filter{(tChar, x, y) => x == currentRow}).flatten.filterNot{ (tChar, x, y) => tChar == ' '}.map{ (tChar, x, y) => y} 
      ((allYLengths.min, allYLengths.max), currentRow) :: getRangeForY(maze, currentRow + 1)

  type EnrichedMaze = Map[Coordinate, CoordinateInfo]
  type Coordinate = (Int, Int)
  type OuterBoxOfSide = (Int, Int, Int, Int)

  trait Orientation
  case object Up extends Orientation
  case object Right extends Orientation
  case object Down extends Orientation
  case object Left extends Orientation

  sealed trait Instruction
  sealed trait Turn extends Instruction
  case object R extends Turn
  case object L extends Turn

  case class Steps(n: Int) extends Instruction
  case class CoordinateInfo(tile: Char, previousX: Int, nextX: Int, previousY: Int, nextY: Int)

  case class Cube(s1: OuterBoxOfSide, s2: OuterBoxOfSide, s3: OuterBoxOfSide, s4: OuterBoxOfSide, s5: OuterBoxOfSide, s6: OuterBoxOfSide):
    def checkIfOutOfBounds(coord: Coordinate, side: Char): Boolean =
      val (minX, maxX, minY, maxY) = side match {
        case 'A' => s1
        case 'B' => s2
        case 'C' => s3
        case 'D' => s4
        case 'E' => s5
        case 'F' => s6
      }
      (coord._1 < minX || coord._1 > maxX || coord._2 < minY || coord._2 > maxY)

  def changeOrientation(t: Turn, o: Orientation): Orientation = 
    t match 
      case R => o match 
        case Up => Right
        case Right => Down 
        case Down => Left
        case Left => Up
      
      case L => o match 
        case Up => Left
        case Right => Up
        case Down => Right
        case Left => Down

  def checkXRange(x: Int, y: Int, xRange: List[((Int, Int), Int)]): (Int, Int) =
    val (minX, maxX) = xRange.filter(_._2 == y).head._1

    if (x >= maxX)
      (maxX - 1, minX)
    else if (x <= minX)
      (maxX, minX + 1)
    else
      (x - 1, x + 1)

  def checkYRange(x: Int, y: Int, yRange: List[((Int, Int), Int)]): (Int, Int) =
    val (minY, maxY) = yRange.filter{_._2 == x}.head._1

    if (y >= maxY)
      (maxY - 1, minY)
    else if (y <= minY)
      (maxY, minY + 1)
    else
      (y - 1, y + 1)

  def makeCircular(input: List[List[(Char, Int, Int)]], xRange: List[((Int, Int), Int)], yRange: List[((Int, Int), Int)]): List[(Coordinate, CoordinateInfo)] =
    def setAllNeighborsForRow(row: List[(Char, Int, Int)]): List[(Coordinate, CoordinateInfo)] =
      row match
        case (tChar, x, y) :: next if (tChar != ' ') => 
          val (prevX, nextX) = checkXRange(x, y, xRange)
          val (prevY, nextY) = checkYRange(x, y, yRange) 
          ((x, y), CoordinateInfo(tChar, prevX, nextX, prevY, nextY)) :: setAllNeighborsForRow(next)     
        case (tChar, x, y) :: next                  => setAllNeighborsForRow(next)
        case Nil                                    => Nil

    input match
      case head :: next => setAllNeighborsForRow(head) ::: makeCircular(next, xRange, yRange)
      case Nil    => Nil

  def executeInstructionPart1(steps: Int, orientation: Orientation, location: Coordinate, maze: EnrichedMaze): Coordinate =
    if (steps == 0)
      location
    else
      val curCoordInfo = maze(location)
      val newCoord = 
        orientation match
          case Up => (location._1, curCoordInfo.previousY)
          case Down => (location._1, curCoordInfo.nextY)
          case Left => (curCoordInfo.previousX, location._2)
          case Right => (curCoordInfo.nextX, location._2)

      if (maze(newCoord).tile == '#')
        location
      else
        executeInstructionPart1(steps - 1, orientation, newCoord, maze)

  def executeInstructionPart2(steps: Int, orientation: Orientation, location: Coordinate, side: Char, maze: EnrichedMaze, cube: Cube): (Coordinate, Char, Orientation) =
    if (steps == 0)
      (location, side, orientation)
    else
      val curCoordInfo = maze(location)
      val (newSide, newCoord, newOrientation) = 
        orientation match
          case Up => 
            if (cube.checkIfOutOfBounds((location._1, location._2 - 1), side))
              wrapAroundForReal(side, location, orientation)
            else
              (side, (location._1, curCoordInfo.previousY), orientation)
          case Down => 
            if (cube.checkIfOutOfBounds((location._1, location._2 + 1), side))
              wrapAroundForReal(side, location, orientation)
            else
              (side, (location._1, curCoordInfo.nextY), orientation)

          case Left =>
            if (cube.checkIfOutOfBounds((location._1 - 1, location._2), side))
              wrapAroundForReal(side, location, orientation) 
            else
              (side, (curCoordInfo.previousX, location._2), orientation)

          case Right =>
            if (cube.checkIfOutOfBounds((location._1 + 1, location._2), side))
              wrapAroundForReal(side, location, orientation) 
            else
              (side, (curCoordInfo.nextX, location._2), orientation)

      if (maze(newCoord).tile == '#')
        (location, side, orientation)
      else
        executeInstructionPart2(steps - 1, newOrientation, newCoord, newSide, maze, cube)

  def moveThroughMaze(maze: EnrichedMaze, instructions: List[Instruction], location: Coordinate, orientation: Orientation): (Coordinate, Orientation) = 
    instructions match 
      case R :: next            => moveThroughMaze(maze, next, location, changeOrientation(R, orientation))
      case L :: next            => moveThroughMaze(maze, next, location, changeOrientation(L, orientation))
      case Steps(moves) :: next => val newCoord = executeInstructionPart1(moves, orientation, location, maze); moveThroughMaze(maze, next, newCoord, orientation)
      case Nil                  => (location, orientation)

  def moveThroughCube(maze: EnrichedMaze, instructions: List[Instruction], location: Coordinate, orientation: Orientation, side: Char, cube: Cube): (Coordinate, Orientation) = 
    instructions match
      case R :: next            => moveThroughCube(maze, next, location, changeOrientation(R, orientation), side, cube)
      case L :: next            => moveThroughCube(maze, next, location, changeOrientation(L, orientation), side, cube)
      case Steps(moves) :: next => val (newCoord, newSide, newOrientation) = executeInstructionPart2(moves, orientation, location, side, maze, cube); moveThroughCube(maze, next, newCoord, newOrientation, newSide, cube)
      case Nil                  => (location, orientation)

  def calculateCoordToAnswer(coord: Coordinate, orientation: Orientation): Int =
    val orientationPoints = 
      orientation match
        case Left   => 2
        case Up     => 3
        case Right  => 0
        case Down   => 1
    
    ((coord._1 * 4) + (coord._2 * 1000) + orientationPoints)

  // ------------------- VARIABLES --------------------------- //

  val mazeWithOnlyTilesAndCoords = input.init.map { listOfInstrOrTile => 
    listOfInstrOrTile.collect{ case tile: (Char, Int) => tile}
  }.zipWithIndex.map{ (tile, y) => 
    tile.map{ (tChar, x) => (tChar, x + 1, y + 1) }
  }

  val rangeXPerRow = mazeWithOnlyTilesAndCoords.map{ row => 
    row.filterNot{ (tChar, x, y) => tChar == ' '}
    .map{ (tChar, x, y) => x}}
    .map{ listOfX => (listOfX.min, listOfX.max)}
    .zipWithIndex.map{ case ((xmin, xmax), y) => ((xmin, xmax), y + 1)}

  val rangeYPerCol = getRangeForY(mazeWithOnlyTilesAndCoords)
  val enrichedMaze = makeCircular(mazeWithOnlyTilesAndCoords, rangeXPerRow, rangeYPerCol).to(scala.collection.mutable.Map)

  val start = enrichedMaze.keys.toList.sortBy{ case (x,y) => (y, x) }.head

  def Day22Part1 =
    val lastPosition = moveThroughMaze(enrichedMaze, instructions, start, Right)
    val answer = calculateCoordToAnswer(lastPosition._1, lastPosition._2)

    println(s"Day 22 - part 1: ${answer}")

  def Day22Part2 =
    def generateOuterBox(startX: Int, startY: Int, maxSize: Int): OuterBoxOfSide = 
      (startX, startX + maxSize, startY, startY + maxSize)

    val cubeSize = 49
    val constructedCube: Cube =
      Cube(s1 = generateOuterBox(51, 1, cubeSize),      
          s2 = generateOuterBox(101, 1, cubeSize), 
          s3 = generateOuterBox(51, 51, cubeSize), 
          s4 = generateOuterBox(1, 101, cubeSize), 
          s5 = generateOuterBox(51, 101, cubeSize), 
          s6 = generateOuterBox(1, 151, cubeSize))

    val lastPosition = moveThroughCube(enrichedMaze, instructions, start, Right, 'A', constructedCube)
    val answer = calculateCoordToAnswer(lastPosition._1, lastPosition._2)

    println(s"Day 22 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day22Part1
  //   Day22Part2
}