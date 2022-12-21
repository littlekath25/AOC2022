package aoc21
import scala.io.Source
import scala.collection.mutable._

object Day00 {
  type Monkey = String
  type Number = Long 
  type Numbers = Map[Monkey, Number]
  type Operation = Char
  type Formula = (Operation, Monkey, Monkey)
  type Formulas = Map[Monkey, Formula]

  def parse: (Numbers, Formulas) =
    val numbers = scala.collection.mutable.Map[Monkey, Number]()
    val formulas = scala.collection.mutable.Map[Monkey, Formula]()

    Source.fromResource("Day21.txt")
    .getLines
    .foreach { l => 
      l match {
        case s"$name: $monkeyName1 + $monkeyName2" => formulas += (name -> ('+', monkeyName1, monkeyName2))
        case s"$name: $monkeyName1 - $monkeyName2" => formulas += (name -> ('-', monkeyName1, monkeyName2))
        case s"$name: $monkeyName1 / $monkeyName2" => formulas += (name -> ('/', monkeyName1, monkeyName2))
        case s"$name: $monkeyName1 * $monkeyName2" => formulas += (name -> ('*', monkeyName1, monkeyName2))
        case s"$name: $number"                     => numbers += (name -> number.toLong); 
    }}

    (numbers, formulas)

  def evaluate(numbers: Numbers, formulas: Formulas): (Numbers, Formulas) = 
    var formulaSize = formulas.size

    formulas.foreach { 
      case (monkey, (op, lhs, rhs)) => 
        val neigh1 = numbers.get(lhs)
        val neigh2 = numbers.get(rhs)

        if (neigh1.isDefined && neigh2.isDefined) {
          val number = (op, neigh1, neigh2) match {
            case ('+', Some(l), Some(r)) => l + r
            case ('-', Some(l), Some(r)) => l - r
            case ('*', Some(l), Some(r)) => l * r
            case ('/', Some(l), Some(r)) => l / r 
            case _ => sys.error("dit kan niet")
          }
          numbers += (monkey -> number)
          formulas -= (monkey)
        }
        else ()
      }

    if (formulas.size == formulaSize)
      (numbers, formulas) 
    else 
    evaluate(numbers, formulas)

  def findShout(numbers: Numbers, formulas: Formulas): Long = 
    var search = "root"
    var result = 0L

    while (search != "humn") {
      val (op, lhs, rhs) = formulas(search)
      val (newSearch, newResult) = (op, numbers.get(lhs), numbers.get(rhs)) match {
        case ('=', None, Some(x)) => (lhs, x)
        case ('=', Some(x), None) => (rhs, x)
        case ('+', None, Some(x)) => (lhs, result - x)
        case ('+', Some(x), None) => (rhs, result - x)
        case ('-', None, Some(x)) => (lhs, result + x)
        case ('-', Some(x), None) => (rhs, x - result)
        case ('*', None, Some(x)) => (lhs, result / x)
        case ('*', Some(x), None) => (rhs, result / x)
        case ('/', None, Some(x)) => (lhs, result * x)
        case ('/', Some(x), None) => (rhs, x / result)
        case _ => sys.error("dit kan óók niet")
      }  
      search = newSearch
      result = newResult
    }
    result


  def Day00Part1 =
    val input = parse
    val evaluated = evaluate(input._1, input._2)
    val answer = evaluated._1.find(_._1 == "root").get

    println(s"Day 21 - part 1: ${answer._2}")

  def Day00Part2 =
    val input = parse
    val newFormulas = input._2.map { case (monkey, formula) => 
      if (monkey == "root") 
        ("root" -> ('=', formula._2, formula._3)) 
      else (monkey, formula)
    }

    val newNumbers = input._1 -= "humn"
    val newMap = (newNumbers, newFormulas)
    
    val evaluated = evaluate(newMap._1, newMap._2)
    val answer = findShout(evaluated._1, evaluated._2)

    println(s"Day 21 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day00Part1
  //   Day00Part2
  }