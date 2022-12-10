package AOC2022
import scala.io.Source
import scala.collection.mutable

object Day09 {
  def Day09Part1 =
    def checkIfTouching(tail: (Int, Int), head: (Int, Int)): Boolean =
      val difference = ((tail._1 - head._1).abs, (tail._2 - head._2).abs)
      val touchingOrNot = !(difference._1 > 1 || difference._2 > 1) || (difference._1 < -1 || difference._2 < -1)
      touchingOrNot

    val input = Source.fromResource("Example.txt").getLines.map{ instr => val splitted = instr.split(" "); (splitted(0), splitted(1).toInt)}.toList
    val max = 1000

    def move(from: (Int, Int), to: (Int, Int)): (Int, Int) =
      if (from._1 < to._1)
        (from._1 + 1, from._2)
      else if (from._1 > to._1)
        (from._1 - 1, from._2)
      else if (from._2 < to._2)
        (from._1, from._2 + 1)
      else
        (from._1, from._2 - 1)

    var bridgeMap = Array.tabulate(max, max)((x, y) => ("."))
    val start = (max - 1, 0)

    val head = (500, 500)
    val tail = head

    def getNewCoords(direction: Char, moves: Int, current: (Int, Int)): (Int, Int) =
      if (direction == 'U')
        (current._1 - moves, current._2)
      else if (direction == 'D')
        (current._1 + moves, current._2)
      else if (direction == 'L')
        (current._1, current._2 - moves)
      else
        (current._1, current._2 + moves)
    
    def moveUntil(bridgeMap: Array[Array[String]], head: (Int, Int), tail: (Int, Int), to: (Int, Int)): (Array[Array[String]], (Int, Int)) =
      bridgeMap(tail._1)(tail._2) = "#"
      if (head != to)
        val newHead = move(head, to)
        if (!checkIfTouching(tail, newHead))
          val newTail = head
          moveUntil(bridgeMap, newHead, newTail, to)
        else
          moveUntil(bridgeMap, newHead, tail, to)
      else
        (bridgeMap, tail)

    val answer = input.foldLeft(bridgeMap, head, tail) {
      case ((bridgemap, head, tail), (direction, moves)) =>
        val newHeadCoords = getNewCoords(direction(0), moves, head)
        val newBridgeMapAndCoords = moveUntil(bridgeMap, head, tail, newHeadCoords)
        ((newBridgeMapAndCoords._1, newHeadCoords, newBridgeMapAndCoords._2))
    }

    println(s"Day 08 - part 1: ${answer._1.flatten.toList.count(_ == "#")}")

  def Day09Part2 =
    case class Move(direction: String, moves: Int)

    case class Knot(position: (Int, Int)) {
      def follow(move: Move): Knot =
        move.direction match {
          case ("U") => Knot(position._1 - 1, position._2)
          case ("D") => Knot(position._1 + 1, position._2)
          case ("L") => Knot(position._1, position._2 - 1)
          case ("R") => Knot(position._1, position._2 + 1)
        }
    }

    val moves = Source.fromResource("Example.txt").getLines
      .map{ instr => 
        val splitted = instr.split(" ");
        Move(splitted(0), splitted(1).toInt)
      }.toList

    var knots = Array.tabulate(10)(x => Knot(0, 0))
    val visited = List((0, 0))

    def checkIfTouching(head: (Int, Int), tail: (Int, Int)): Boolean =
      val difference = ((head._1.abs - tail._1.abs), (head._2.abs - tail._2.abs))
      // println(s"CHECK IF TOUCHING $head AND $tail - DIFFERENCE: $difference")
      val touchingOrNot = !(difference._1 > 1 || difference._2 > 1)
      touchingOrNot

    def moveToKnot(previous: Knot, current: Knot): Array[Knot] = 
      val distance = ((previous.position._1.abs - current.position._1.abs), (previous.position._2.abs - current.position._2.abs))
      val movedKnot = distance match {
        case (distance) => 
          if (distance._1 == 0 && distance._2 == 2) 
            Knot(current.position._1, current.position._2 + 1)
          else if (distance._1 == 0 && distance._2 == -2) 
            Knot(current.position._1, current.position._2 - 1)
          else if (distance._1 == 2 && distance._2 == 0) 
            Knot(current.position._1 - 1, current.position._2)
          else if (distance._1 == -2 && distance._2 == 0) 
            Knot(current.position._1 + 1, current.position._2) 

          else if (distance._1 > 0 && distance._2 > 0) 
            Knot(current.position._1 - 1, current.position._2 + 1)
          else if (distance._1 < 0 && distance._2 < 0) 
            Knot(current.position._1 + 1, current.position._2 - 1)
          else if (distance._1 > 0 && distance._2 < 0) 
            Knot(current.position._1 - 1, current.position._2 - 1)
          else
            Knot(current.position._1 + 1, current.position._2 + 1)
      }
      Array(movedKnot)

    def stayOrMove(knots: Array[Knot], visited: List[(Int, Int)]): (Array[Knot], List[(Int, Int)]) =
      var knotsTail = knots.drop(1)
      var knotsHead = knots(0)
      val tailTouchingHead = checkIfTouching(knotsHead.position, knotsTail(0).position)

      if (!tailTouchingHead)
        knotsTail = knotsTail.foldLeft(Array(knotsHead)) { (listKnots, currentKnot) =>
          val previousKnot = listKnots.last
          val touchingPrevious = checkIfTouching(previousKnot.position, currentKnot.position)
          if (touchingPrevious)
            listKnots.concat(Array(currentKnot))
          else
            listKnots.concat(moveToKnot(previousKnot, currentKnot))
        }
        
        println(s"AFTER NOT TOUCHING")
        knotsTail.foreach {
          println
        }
        println

        (knotsTail, visited ::: List(knotsTail.last.position))
      else
        println(s"ALREADY TOUCHING")
        knots.foreach {
          println
        }
        println

        (knots, visited)

    def moveTheSnake(knots: Array[Knot], move: Move, visited: List[(Int, Int)]): (Array[Knot], List[(Int, Int)]) =
      if (move.moves > 0)
        val newKnotsAndVisited = stayOrMove(knots.updated(0, knots(0).follow(move)), visited)
        moveTheSnake(newKnotsAndVisited._1, Move(move.direction, move.moves - 1), newKnotsAndVisited._2)
      else
        (knots, visited)

    val answer = moves.foldLeft(knots, visited) {
      case ((knots, visited), move) =>
        println(s"THE MOVE IS: $move")
        moveTheSnake(knots, move, visited)
    }

    println(s"Day 08 - part 2: ${answer._2.distinct}")

  // def main(args: Array[String]): Unit =
  //   Day09Part1
  //   Day09Part2
}
