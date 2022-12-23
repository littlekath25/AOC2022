package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day23 {
  val deltas = List(
    Pos(0, -1), // 0) N
    Pos(0, 1), // 1) S
    Pos(1, 0), // 2) E
    Pos(- 1, 0),  // 3) W
    Pos(1, - 1), // 4) NE
    Pos(- 1, - 1), // 5) NW
    Pos(1, 1), // 6) SE
    Pos(- 1, 1)) // 7) SW

  case class Pos(x: Int, y: Int) {
    def plus(delta: Pos): Pos = Pos(this.x + delta.x, this.y + delta.y)
  }

  def neighborsOccupied(neighbors: List[Pos], grove: Map[Pos, Char]): List[Boolean] =
    neighbors.map { neighbor =>
      val maybeNeighbor = grove.get(neighbor)
      if ((maybeNeighbor.isDefined && maybeNeighbor.get == '.') || maybeNeighbor.isEmpty)
        false
      else
        true
    }

  def freeToMove(elf: Pos, grove: Map[Pos, Char]): Boolean =
    val neighbors = deltas.map { delta =>
      elf.plus(delta)
    }
    neighborsOccupied(neighbors, grove).exists(_ == true)
  
  def checkNorth(elf: Pos, grove: Map[Pos, Char]): Option[Pos] =
    val neighbors = List(elf.plus(deltas(0)), elf.plus(deltas(4)), elf.plus(deltas(5))) 
    if (neighborsOccupied(neighbors, grove).exists(_ == true))
      None
    else
      Some(Pos(elf.x, elf.y - 1))

  def checkSouth(elf: Pos, grove: Map[Pos, Char]): Option[Pos] =
    val neighbors = List(elf.plus(deltas(1)), elf.plus(deltas(6)), elf.plus(deltas(7))) 
    if (neighborsOccupied(neighbors, grove).exists(_ == true))
      None
    else
      Some(Pos(elf.x, elf.y + 1))

  def checkWest(elf: Pos, grove: Map[Pos, Char]): Option[Pos] =
    val neighbors = List(elf.plus(deltas(3)), elf.plus(deltas(5)), elf.plus(deltas(7))) 
    if (neighborsOccupied(neighbors, grove).exists(_ == true))
      None
    else
      Some(Pos(elf.x - 1, elf.y))

  def checkEast(elf: Pos, grove: Map[Pos, Char]): Option[Pos] =
    val neighbors = List(elf.plus(deltas(2)), elf.plus(deltas(4)), elf.plus(deltas(6))) 
    if (neighborsOccupied(neighbors, grove).exists(_ == true))
      None
    else
      Some(Pos(elf.x + 1, elf.y))

  val checks = List (
      checkNorth,
      checkSouth,
      checkWest,
      checkEast
    )
    
  def checkWhatElvesCanMove(grove: Map[Pos, Char]): Map[Pos, Char] =
    grove.filter { (position, tileChar) =>
      (tileChar == '#' && freeToMove(position, grove) == true)
    }

  def proposeForEachElf(grove: Map[Pos, Char], elves: Map[Pos, Char], currentChecks: List[(Pos, Map[Pos, Char]) => Option[Pos]]): Map[Pos, Pos] =
    val proposal = scala.collection.mutable.Map[Pos, Pos]()

    elves.map { elf =>
      val options = currentChecks.map { check => check(elf._1, grove) }.filter(_.nonEmpty)
      if (options.nonEmpty)
        proposal += (elf._1 -> options.head.get)
    }
    proposal

  def moveElves(grove: Map[Pos, Char], elfProposals: Map[Pos, Pos]): Map[Pos, Char] =
    val allProposalValues = elfProposals.map(_._2).toList

    elfProposals.foreach { proposal =>
      if (allProposalValues.count(_ == proposal._2) == 1)
        grove += ((proposal._2) -> '#')
        grove(proposal._1) = '.'
    }
    grove

  def goRounds(grove: Map[Pos, Char], checks: List[(Pos, Map[Pos, Char]) => Option[Pos]], round: Int = 1): Map[Pos, Char] =
    if (round <= 10000)
      val elvesThatAreFreeToMove = checkWhatElvesCanMove(grove)
      val proposal = proposeForEachElf(grove, elvesThatAreFreeToMove, checks)
      if (proposal.isEmpty)
        println(s"ROUND: $round")
        return (grove)
      val newGrove = moveElves(grove, proposal)
      val newChecks = checks.tail :+ checks.head
      goRounds(newGrove, newChecks, round + 1)
    else
      grove

  def Day23Part1 =
    val grove = Source.fromResource("Day23.txt").getLines
    .map{ _.toList.zipWithIndex }
    .zipWithIndex.flatMap { (row, y) => 
      row.map { ( tChar, x) =>
        (Pos(x + 1, y + 1) -> tChar)
    }}.to(scala.collection.mutable.Map)

    goRounds(grove, checks)

    val (xMin, xMax) = (grove.map(_._1).toList.minBy(_.x).x, grove.map(_._1).toList.maxBy(_.x).x)
    val (yMin, yMax) = (grove.map(_._1).toList.minBy(_.y).y, grove.map(_._1).toList.maxBy(_.y).y)

    val realXTotal = xMax - xMin + 1
    val realYTotal = yMax - yMin + 1

    val totalElves = grove.map(_._2).filter(_ == '#').size

    println(s"total: ${realXTotal * realYTotal - totalElves}")

  // def Day23Part2 =
  //   val answer = ???

  //   println(s"Day 23 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day23Part1
  //   Day23Part2
}