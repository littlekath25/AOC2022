// package AOC2022
// import scala.io.Source
// import scala.collection.mutable._

// object Day24 {
//   val input = Source.fromResource("Example.txt").getLines.map(_.toArray).toArray

//   case class Pos(x: Int, y: Int) {
//     def move(dir: Char): Pos =
//       dir match
//         case '^' => Pos(x, y - 1)
//         case '>' => Pos(x + 1, y)
//         case 'v' => Pos(x, y + 1)
//         case '<' => Pos(x - 1, y)
//   }

//   def checkOutOfBounds(pos: Pos, cave: Array[Array[ArrayBuffer[Char]]]): Boolean =
//     (pos.x < 0 || pos.x > cave.head.size - 1 || pos.y < 0 || pos.y > cave.size - 1)

//   def wrapAround(pos: Pos, cave: Array[Array[ArrayBuffer[Char]]]): Pos =
//     if (checkOutOfBounds(pos, cave))
//       pos

//     else
//       var newX = pos.x
//       var newY = pos.y

//       if (pos.y == 0)
//         newY = cave.size - 2

//       else if (pos.y == cave.size - 1)
//         newY = 1

//       if (pos.x == 0)
//         newX = cave.head.size - 2

//       else if (pos.x == cave.head.size - 1)
//         newX = 1

//       Pos(newX, newY)

//   def endReached(current: Pos, end: Pos): Boolean =
//     (current == end)

//   def moveBlizzard(cave: Array[Array[ArrayBuffer[Char]]], curr: Pos, dir: Char): Pos =
//     (wrapAround(curr.move(dir), cave))

//   def updateBlizzards(cave: Array[Array[ArrayBuffer[Char]]]): Array[Array[ArrayBuffer[Char]]] =
//     val newShitstorm = cave.map { row =>
//       row.map { col =>
//         ArrayBuffer.empty[Char]
//       }
//     }

//     for (y <- 0 to cave.size - 1) do
//     {
//       for (x <- 0 to cave.head.size - 1) do
//       {
//         val currPos = Pos(x, y)
//         val charsOnThisTile = cave(y)(x)
//         charsOnThisTile.foreach { blizzard =>
//           if (blizzard != '.')
//             if (blizzard == '#')
//               newShitstorm(currPos.y)(currPos.x) += blizzard
//             else
//               val newMove = moveBlizzard(cave, currPos, blizzard)
//               newShitstorm(newMove.y)(newMove.x) += blizzard
//         }
//       }
//     }

//     newShitstorm

//   def getPosForKatherine(shitStorm: Array[Array[ArrayBuffer[Char]]], current: Pos): ArrayBuffer[Pos] =
//     val possibleMoves = ArrayBuffer.empty[Pos]

//     var neighbors = ArrayBuffer (
//       Pos(current.y, current.x),
//       Pos(current.y, current.x - 1),
//       Pos(current.y, current.x + 1),
//       Pos(current.y - 1, current.x),
//       Pos(current.y + 1, current.x)
//     )

//     neighbors.foreach { neighbor =>
//       if (!checkOutOfBounds(neighbor, shitStorm))
//         val tile = shitStorm(neighbor.y)(neighbor.x)
//         if (tile.size == 0)
//           possibleMoves += neighbor
//     }
//     possibleMoves

//   def sheARunnerShesATrackstar(blizzards: Array[Array[ArrayBuffer[Char]]], start: Pos, end: Pos): Int = 
//     var kathsPos = ArrayBuffer(start)
//     var updatedBlizzard = blizzards

//     var time = 0
//     while (time < 1000) {
//       time = time + 1

//       val visitedPositions = blizzards.map { row =>
//         Array.fill(row.size)(false)
//       }

//       val newPositions = ArrayBuffer.empty[Pos]

//       kathsPos.foreach { pos =>
//         val possibleMoves = getPosForKatherine(updatedBlizzard, pos)
//         if (possibleMoves.size > 0)
//           possibleMoves.foreach { pos =>
//             visitedPositions(pos.y)(pos.x) = true
//           }
//       }

//       for (y <- 0 to blizzards.size - 1) do
//       {
//         for (x <- 0 to blizzards.head.size - 1) do
//         {
//           val newPosition = Pos(x, y)
//           if (visitedPositions(newPosition.y)(newPosition.x))
//             if (endReached(newPosition, end))
//               return time
//             newPositions += newPosition
//         }
//       }

//       updatedBlizzard = updateBlizzards(updatedBlizzard)
//       kathsPos = newPositions
//     }
//     -1

//   def printBlizzard(blizzard: Array[Array[ArrayBuffer[Char]]]): Unit = 
//     blizzard.foreach { row =>
//       row.foreach { tile =>
//         tile.foreach(print)
//         print(" ")
//       }
//       println
//     }

//   def Day24Part1 =
//     val blizzards = input.map { row =>
//       row.map { tile =>
//         ArrayBuffer(tile)
//       }
//     }

//     val start = Pos(0, 1)
//     val end = Pos(blizzards.head.size - 2, blizzards.size - 2)

//     val answer1 = sheARunnerShesATrackstar(blizzards, start, end)
//     println(s"Day 00 - part 1: ${answer1}")

//   // def Day24Part2 =
//   //   val answer = ???

//   //   println(s"Day 00 - part 2: ${answer}")

//   def main(args: Array[String]): Unit =
//     Day24Part1
//     // Day24Part2
// }