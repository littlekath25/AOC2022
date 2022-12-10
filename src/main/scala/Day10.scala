package AOC2022
import scala.io.Source
import java.io._
object Day10 {
  val input = Source.fromResource("Day10.txt").getLines.toList
  
  def Day10Part1 =
    val round = 1
    var totalSignal = 1

    def check(round: Int, totalSignal: Int): List[Int] =
      if ((round == 20) || ((round - 20) % 40 == 0))
        List(round * totalSignal)
      else
        List()

    val answer = input.foldLeft(round, totalSignal, List[Int]()) {
      case (scores, instr) =>
        instr match {
          case (s"noop") =>
            (scores._1 + 1, scores._2, scores._3.concat(check(scores._1, scores._2)))
          case (s"addx $value") => 
            val firstStep = (scores._1 + 1, scores._2, scores._3.concat(check(scores._1, scores._2)))
            (scores._1 + 2, scores._2 + value.toInt, firstStep._3.concat(check(firstStep._1, scores._2)))
        }
    }

    println(s"Day 10 - part 1: ${answer._3.sum}")


  def Day10Part2 =
    case class Sprite(pos: Int)

    def check(value: Int, sprite: Sprite): Boolean =
      val litOrNah = value == sprite.pos || value == (sprite.pos - 1) || value == (sprite.pos + 1)
      litOrNah
      
    def drawOrNot(sprite: Sprite, pos: Int, grid: Array[Char]): (Sprite, Int, Array[Char]) =
      if (check(pos % 40, sprite))
        (sprite, pos + 1, grid.updated((pos), '#'))
      else
        (sprite, pos + 1, grid)

    def updateSprite(pos: Int): Sprite = 
      Sprite(pos)
 
    val sprite = Sprite(1)
    var grid = Array.tabulate(240)(x => ' ')

    val answer = input.foldLeft((sprite, 0, grid), 1) {
      case (info, instr) =>
        instr match {
          case (s"noop") =>
            (drawOrNot(info._1._1, info._1._2, info._1._3), info._2)
          case (s"addx $value") =>
            val firstStep = (drawOrNot(info._1._1, info._1._2, info._1._3), info._2)
            val secondStep = (drawOrNot(info._1._1,  firstStep._1._2, firstStep._1._3), info._2)
            val newSprite = updateSprite(secondStep._2 + value.toInt)
            val stars = info._2 + value.toInt
            ((newSprite, secondStep._1._2, secondStep._1._3), stars)
        }
    }

    val answerFile = new PrintWriter(new File("src/main/resources/Day10-answer.txt"))
    answer._1._3.grouped(40).toList.foreach { line =>
      answerFile.write(line)
      answerFile.write("\n")
    }
    answerFile.close

    println(s"Day 10 - part 2: Day10-answer.txt ")

  // def main(args: Array[String]): Unit =
  //   Day10Part1
  //   Day10Part2
}
