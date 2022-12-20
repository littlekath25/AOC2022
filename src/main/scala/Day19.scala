package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day19 {
  enum RobotType:
    case OreRobot
    case ClayRobot
    case ObsidianRobot
    case GeodeRobot
    case Harvest

  import RobotType._

  final case class Robot(version: RobotType)
  final case class Blueprint(minerals: Minerals, robots: List[RobotType], shop: Map[RobotType, Minerals]) {
    val highestOreCost = shop.map{item => item._2.ore}.max

    def harvest: Minerals =
      robots.map { robot =>
        robot match {
        case OreRobot       => Minerals(1, 0, 0, 0)
        case ClayRobot      => Minerals(0, 1, 0, 0)
        case ObsidianRobot  => Minerals(0, 0, 1, 0)
        case GeodeRobot     => Minerals(0, 0, 0, 1)
        case Harvest        => minerals
      }}.foldLeft(Minerals(0, 0, 0, 0))(_ plus _)
 
    def buildRobot(action: RobotType): List[RobotType] =
      action match {
        case OreRobot      => robots.appended(OreRobot)
        case ClayRobot     => robots.appended(ClayRobot)
        case ObsidianRobot => robots.appended(ObsidianRobot)
        case GeodeRobot    => robots.appended(GeodeRobot)
        case Harvest       => robots
      }

    def enoughOre(robot: RobotType): Boolean =
      minerals.ore >= shop(robot).ore

    def generateActions: List[RobotType] =
      val oreRobot: Option[RobotType] = if (enoughOre(OreRobot) && robots.count(_ == OreRobot) <= (highestOreCost)) Some(OreRobot) else None
      val clayRobot: Option[RobotType] = if (enoughOre(ClayRobot) && robots.count(_ == ClayRobot) <= shop(ObsidianRobot).clay) Some(ClayRobot) else None
      val obsidianRobot: Option[RobotType] = if (enoughOre(ObsidianRobot) && minerals.clay >= shop(ObsidianRobot).clay) Some(ObsidianRobot) else None
      val geodeRobot: Option[RobotType] = if (enoughOre(GeodeRobot) && minerals.obsidian >= shop(GeodeRobot).obsidian) Some(GeodeRobot) else None

      if (minerals.obsidian >= shop(GeodeRobot).obsidian)
        List(geodeRobot, Some(Harvest)).flatten
      else if (minerals.clay >= shop(ObsidianRobot).clay)
        List(geodeRobot, obsidianRobot, Some(Harvest)).flatten
      else if (minerals.ore > highestOreCost)
        List(oreRobot, clayRobot, obsidianRobot, geodeRobot).flatten
      else
        List(oreRobot, clayRobot, obsidianRobot, geodeRobot, Some(Harvest)).flatten
    }

  final case class Minerals(ore: Int, clay: Int, obsidian: Int, geode: Int) {
    def plus(other: Minerals): Minerals =
      Minerals(this.ore + other.ore, this.clay + other.clay, this.obsidian + other.obsidian, this.geode + other.geode)

    def minus(other: Minerals): Minerals =
      Minerals(this.ore - other.ore, this.clay - other.clay, this.obsidian - other.obsidian, this.geode)
  }

  val blueprints = Source.fromResource("Day19.txt").getLines.map { line => line match
    case (s"Blueprint $name: Each ore robot costs $ore ore. Each clay robot costs $clay ore. Each obsidian robot costs $obsidianOre ore and $obsidianClay clay. Each geode robot costs $geodeOre ore and $geodeObsidian obsidian.") =>
      (name.toInt -> Blueprint(
        Minerals(0, 0, 0, 0),
        List(OreRobot),
        Map(
          (OreRobot -> Minerals(ore.toInt, 0, 0, 0)),
          (ClayRobot -> Minerals(clay.toInt, 0, 0, 0)),
          (ObsidianRobot -> Minerals(obsidianOre.toInt, obsidianClay.toInt, 0, 0)),
          (GeodeRobot -> Minerals(geodeOre.toInt, 0, geodeObsidian.toInt, 0))
        )
      ))
  }to(scala.collection.mutable.Map)

  def getMaxGeodes(blueprint: Blueprint, previousGeodes: Int, limit: Int): Int =
    val currentGeodes = (blueprint.minerals.geode + blueprint.robots.count(_ == GeodeRobot))
    val newMaxGeodes =
      if (currentGeodes > previousGeodes) 
        currentGeodes
      else
        previousGeodes

    if (limit > 0)
      val newActions = blueprint.generateActions
      newActions.foldLeft(newMaxGeodes)((maxGeodes, action) =>
        if (action == Harvest)
          val harvestedMinerals = blueprint.harvest.plus(blueprint.minerals)
          val calculatedGeodes = getMaxGeodes(blueprint.copy(minerals = harvestedMinerals), newMaxGeodes, limit - 1)
          if (calculatedGeodes > maxGeodes) calculatedGeodes else maxGeodes
        else
          val newRobots = blueprint.buildRobot(action)
          val newMinerals = blueprint.minerals.minus(blueprint.shop(action))
          val harvestedMinerals = blueprint.harvest.plus(newMinerals)
          val calculatedGeodes = getMaxGeodes(blueprint.copy(minerals = harvestedMinerals, robots = newRobots), newMaxGeodes, limit - 1)
          if (calculatedGeodes > maxGeodes) calculatedGeodes else maxGeodes
      )
      else
        currentGeodes

  // def Day19Part1 =
  //   val answer = blueprints.map(print => getMaxGeodes(print._2, 0, 23) * print._1).sum
  //   println(s"Day 19 - part 1: ${answer}")

  // def Day19Part2 =
  //   val answer = blueprints.take(3).map(print => getMaxGeodes(print._2, 0, 31)).product
  //   println(s"Day 19 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day19Part1
  //   Day19Part2
}
