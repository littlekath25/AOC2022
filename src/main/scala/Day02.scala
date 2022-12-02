package AOC2022
import scala.io.Source

object Day02 {
  val input: List[(String, String)] = Source.fromResource("Day02.txt").getLines.map{ 
    x => val splitted = x.split(" "); 
    (splitted(0), splitted(1))
  }.toList

  def Day02Part1 = 
    def matchPoint(opp: String, me: String) : Int =
      opp match {
        case (x) if (x == "A") =>
          me match {
            case y if (y == "X") => 3
            case y if (y == "Y") => 6
            case y if (y == "Z") => 0
          }
        case (x) if (x == "B") =>
          me match {
            case y if (y == "X") => 0
            case y if (y == "Y") => 3
            case y if (y == "Z") => 6
          }
        case (x) if (x == "C") =>
          me match {
            case y if (y == "X") => 6
            case y if (y == "Y") => 0
            case y if (y == "Z") => 3
          }
      }

    val answer = input.foldLeft(0){
      case ((points), (opp, me)) => 
        if (me == "X")
          (points + 1 + matchPoint(opp, me)) 
        else if (me == "Y")
          (points + 2 + matchPoint(opp, me)) 
        else
          (points + 3 + matchPoint (opp, me))
    }

    println(s"Day 2 - part 1: $answer")

  def Day02Part2 = 
    def matchPoint(opp: String, me: String) : Int =
      opp match {
        case (x) if (x == "A") =>
          me match {
            case y if (y == "X") => 0 + 3
            case y if (y == "Y") => 3 + 1
            case y if (y == "Z") => 6 + 2
          }
        case (x) if (x == "B") =>
          me match {
            case y if (y == "X") => 0 + 1
            case y if (y == "Y") => 3 + 2
            case y if (y == "Z") => 6 + 3
          }
        case (x) if (x == "C") =>
          me match {
            case y if (y == "X") => 0 + 2
            case y if (y == "Y") => 3 + 3
            case y if (y == "Z") => 6 + 1
          }
      }

    val answer = input.foldLeft(0){
      case ((points), (opp, me)) => 
        if (me == "X")
          (points + matchPoint(opp, me)) 
        else if (me == "Y")
          (points + matchPoint(opp, me)) 
        else
          (points + matchPoint (opp, me))
    }

    println(s"Day 2 - part 2: $answer")

  def main(args: Array[String]): Unit =
    Day02Part1
    Day02Part2
}
