package AOC2022
import scala.io.Source
import scala.collection.mutable.Stack

object Day11 {
  var modulus = 0

  val input = Source.fromResource("Day11.txt").getLines.toList.map(_.trim).filter(_.nonEmpty).grouped(6).toList
  
  case class Monkey(name: Int, items: Stack[Long], operation: (String, Int), test: Int, ifTrue: Int, ifFalse: Int, inspected: Int)

  def testItem(one: Long, two: Long): Boolean =
    if (one % two == 0)
      true
    else
      false

  def operationForItems(one: Long, two: Long, instr: String): Long =
    var numberToDo = two
    if (two == -1)
      numberToDo = one
    val newValue =
      if (instr == "+")
        one + numberToDo
      else
        one * numberToDo
    // println(s"NEW VALUE: $one $instr $two -> ${newValue / 3}")
    newValue % modulus

  def parseInput(input: List[List[String]], monkeys: List[Monkey]): List[Monkey] =
    input match {
      case  head :: next => 
        val newMonkey = head.map(_.trim).foldLeft(Monkey(0, Stack.empty, (" ", 0), 0, 0, 0, 0)) {
          case (monkey, instr) =>
            instr match {
              case (s"Monkey ${newName}:") => 
                monkey.copy(name = newName.toInt)

              case (s"Starting items: ${newItems}") => 
                val splitted = newItems.split(",").toList.map(_.trim.toLong)
                val newStack = scala.collection.mutable.Stack[Long]()
                newStack.pushAll(splitted)
                monkey.copy(items = newStack)

              case (s"Operation: new = old ${operation}") =>
                val splitted = operation.split(" ").toList
                if (splitted(1) == "old")
                  monkey.copy(operation = (splitted.head, -1))
                else
                  monkey.copy(operation = (splitted.head, splitted(1).toInt))

              case (s"Test: divisible by ${number}") =>
                monkey.copy(test = number.toInt)

              case (s"If true: throw to monkey $toMonkey") =>
                monkey.copy(ifTrue = toMonkey.toInt)

              case (s"If false: throw to monkey $toMonkey") =>
                monkey.copy(ifFalse = toMonkey.toInt)
            }
        }
        parseInput(next, monkeys.concat(List(newMonkey)))
      case Nil => monkeys
    }

  def singleMonkeyRound(monkey: Monkey, groupOfMonkeys: List[Monkey]): List[Monkey] =
    val currentItems = monkey.items.reverse
    val itemsToInspect = monkey.items.size
    val newCurrentItem = currentItems.map { item =>
      val newItem = operationForItems(item, monkey.operation._2, monkey.operation._1)
      if (testItem(newItem, monkey.test))
        groupOfMonkeys(monkey.ifTrue).items.push(newItem)
      else
        groupOfMonkeys(monkey.ifFalse).items.push(newItem)
      }

    monkey.items.popAll
    groupOfMonkeys.updated(monkey.name, monkey.copy(inspected = monkey.inspected + itemsToInspect))

  def goThroughMonkeys(monkeys: List[Monkey]) : List[Monkey] =
    monkeys.foldLeft(monkeys) {
      case (group, monkey) => 
        // println(s"NOW AT MONKEY: ${monkey.name}")
        val newSetup = singleMonkeyRound(monkey, group)
        newSetup
    }

  def goRounds(monkeys: List[Monkey], rounds: Int): List[Monkey] =
    if (rounds > 0)
      goRounds(goThroughMonkeys((monkeys)), rounds - 1)
    else
      monkeys

  def Day11Part1 =
    val monkeys = parseInput(input, List.empty)
    modulus = 3
    val newMonkeyList = goRounds(monkeys, 20).map(monkey => monkey.inspected).sorted.reverse

    println(s"Day 11 - part 1: ${newMonkeyList(0) * newMonkeyList(1)}")

  def Day11Part2 =
    val monkeys = parseInput(input, List.empty)
    modulus = monkeys.map(_.test).product
    val newMonkeyList = goRounds(monkeys, 10000).map(monkey => monkey.inspected).sorted

    println(s"Day 11 - part 2: ${newMonkeyList}")

  def main(args: Array[String]): Unit =
    Day11Part1
    Day11Part2
}
