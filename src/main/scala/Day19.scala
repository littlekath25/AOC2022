package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day19 {
  enum RobotType:
    case OreRobot
    case ClayRobot
    case ObsidianRobot
    case GeodeRobot

  final case class Blueprint(minerals: Minerals, robots: List[Robot], shop: Map[RobotType, Minerals]) {
    def harvest: Minerals =
      val collected = robots.map { robot =>
        robot.version match {
        case (RobotType.OreRobot) => Minerals(1, 0, 0, 0)
        case (RobotType.ClayRobot) => Minerals(0, 1, 0, 0)
        case (RobotType.ObsidianRobot) => Minerals(0, 0, 1, 0)
        case (RobotType.GeodeRobot) => Minerals(0, 0, 0, 1)
      }}.foldLeft(Minerals(0, 0, 0, 0))(_ plus _)
      collected

    def getProduction: (Int, Int, Int) =
      (robots.count(_.version == RobotType.OreRobot),
       robots.count(_.version == RobotType.ClayRobot),
       robots.count(_.version == RobotType.ObsidianRobot)
      )

    def isEnoughToBuild(robot: RobotType): Boolean =
      val enough = (shop(robot).ore <= minerals.ore && shop(robot).clay <= minerals.clay && shop(robot).obsidian <= minerals.obsidian)
      enough

    def producibleRobots: List[RobotType] =
      var listOfAvailableBuilds = List[RobotType]()

      shop.map { robot =>
        if (isEnoughToBuild(robot._1))
          listOfAvailableBuilds = listOfAvailableBuilds.appended(robot._1)
      }
      listOfAvailableBuilds
      
    def buildRobot(robot: RobotType): List[Robot] =
      this.robots.concat(List(Robot(robot)))
    }

  final case class Minerals(ore: Int, clay: Int, obsidian: Int, geode: Int) {
    def plus(other: Minerals): Minerals =
      Minerals(this.ore + other.ore, this.clay + other.clay, this.obsidian + other.obsidian, this.geode + other.geode)

    def minus(other: Minerals): Minerals =
      Minerals(this.ore - other.ore, this.clay - other.clay, this.obsidian - other.obsidian, this.geode)
  }

  final case class Robot(version: RobotType)
  final case class Item(item: RobotType, cost: Minerals)

  val blueprints = Source.fromResource("Example.txt").getLines.map { line => line match
    case (s"Blueprint $name: Each ore robot costs $ore ore. Each clay robot costs $clay ore. Each obsidian robot costs $obsidianOre ore and $obsidianClay clay. Each geode robot costs $geodeOre ore and $geodeObsidian obsidian.") =>
      (name.toInt -> Blueprint(
        Minerals(0, 0, 0, 0),
        List(Robot(RobotType.OreRobot)),
        Map(
          (RobotType.OreRobot -> Minerals(ore.toInt, 0, 0, 0)),
          (RobotType.ClayRobot -> Minerals(clay.toInt, 0, 0, 0)),
          (RobotType.ObsidianRobot -> Minerals(obsidianOre.toInt, obsidianClay.toInt, 0, 0)),
          (RobotType.GeodeRobot -> Minerals(geodeOre.toInt, 0, geodeObsidian.toInt, 0))
        )
      ))
  }to(scala.collection.mutable.Map)

  def oneRound(blueprint: Blueprint, limit: Int): Blueprint = 
    var collectedMinerals = blueprint.minerals
    var newBlueprint = blueprint
    if (limit > 0)

      val production = newBlueprint.getProduction
      val producible = newBlueprint.producibleRobots

      if (producible.contains(RobotType.GeodeRobot))
        newBlueprint = blueprint.copy(minerals = newBlueprint.minerals.minus(newBlueprint.shop(RobotType.GeodeRobot)), robots = newBlueprint.buildRobot(RobotType.GeodeRobot))
      else if (producible.contains(RobotType.ObsidianRobot) && newBlueprint.shop(RobotType.GeodeRobot).obsidian - 1 > production._3)
        newBlueprint = blueprint.copy(minerals = newBlueprint.minerals.minus(newBlueprint.shop(RobotType.ObsidianRobot)), robots = newBlueprint.buildRobot(RobotType.ObsidianRobot))
      else if (producible.contains(RobotType.OreRobot) && newBlueprint.shop(RobotType.ObsidianRobot).ore -1 > production._1)
        newBlueprint = blueprint.copy(minerals = newBlueprint.minerals.minus(newBlueprint.shop(RobotType.OreRobot)), robots = newBlueprint.buildRobot(RobotType.OreRobot))
      else if (producible.contains(RobotType.ClayRobot) && newBlueprint.shop(RobotType.ObsidianRobot).clay / 3 - 1 > production._2)
        newBlueprint = blueprint.copy(minerals = newBlueprint.minerals.minus(newBlueprint.shop(RobotType.ClayRobot)), robots = newBlueprint.buildRobot(RobotType.ClayRobot))

      // println(s"AFTER BUILDING ROBOT: ${newBlueprint.minerals}")
      collectedMinerals = blueprint.harvest.plus(newBlueprint.minerals)
      // println(s"ROBOTS: ${blueprint.robots}")
      // println(s"NEW AFTER HARVEST: ${collectedMinerals}")

      newBlueprint = newBlueprint.copy(minerals = collectedMinerals)
      oneRound(newBlueprint, limit - 1)
    
    else
      newBlueprint

  def Day19Part1 =
    val answer = oneRound(blueprints(1), 24)

    println(s"Day 19 - part 1: ${answer.minerals}")

  // def Day19Part2 =
  //   val answer = ???

  //   println(s"Day 19 - part 2: ${answer}")

  def main(args: Array[String]): Unit =
    Day19Part1
  //   Day19Part2
}
