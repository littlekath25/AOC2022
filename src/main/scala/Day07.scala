package AOC2022
import scala.io.Source
import scala.util.matching.Regex 

object Day07 {
  type FileSystem =  Map[String, List[Item]]
  case class Item(name: String, isDirectory: Boolean, size: Option[Int])

  val input = Source.fromResource("Day07.txt").getLines.toList

  def getNewPath(currentPath: String, to: String) : String =
    if (currentPath == to) 
      currentPath 
    else if (to == "..") 
      currentPath.substring(0, currentPath.lastIndexWhere(_ == '/')) 
    else if (currentPath == "/") 
      currentPath + to 
    else currentPath + "/" + to

  def createNewItems(currentPath: String, input: List[String], filesystem: FileSystem): FileSystem =
    var newSystem = filesystem
    if (!filesystem.exists(_._1 == currentPath))
      newSystem = filesystem + (currentPath -> List.empty)
    input match {
      case head :: next => head match {
        case s"$$ cd $to" =>
          mapOverInput(currentPath, input, newSystem)
        case s"dir $dir" =>
          val newItem = newSystem(currentPath) ::: List(Item(dir, true, None))
          createNewItems(currentPath, next, newSystem + (currentPath -> newItem))
        case s"$size $file" =>
          val newItem = newSystem(currentPath) ::: List(Item(file, false, Some(size.toInt)))
          createNewItems(currentPath, next, newSystem + (currentPath -> newItem))
        }
    case _ => newSystem
  }

  def mapOverInput(currentPath: String, input: List[String], filesystem: FileSystem): FileSystem =
    input match {
      case head :: next => head match {
        case s"$$ cd $to" =>
          val newPath = getNewPath(currentPath, to)
          mapOverInput(newPath, next, filesystem)
        case s"$$ ls" =>
          createNewItems(currentPath, next, filesystem)
        case _ =>
          mapOverInput(currentPath, next, filesystem)
        }
    case _ => filesystem
  }

  val startingMap = Map("/" -> List.empty)
  val newMap = mapOverInput("/", input, startingMap)
  val summedMap = newMap.map { directory =>
    (directory._1, directory._2.filter(_.size != None))
  }.map { directory =>
    (directory._1, directory._2.map(_.size.get).sum)
  }

  val summedSummerMap = summedMap.map { directory =>
      val allKeys = summedMap.filter(_._1.startsWith(directory._1))
      allKeys.foldLeft(0)(_ + _._2)
    }

  def Day07Part1 =
    val answer = summedSummerMap.filter(_ < 100000).sum

    println(s"Day 07 - part 1: ${answer}")

  def Day07Part2 =
    val spaceNeed = 30000000 - (70000000 - summedSummerMap.max)
    val answer = summedSummerMap.filter(_ > spaceNeed).min

    println(s"Day 07 - part 2: ${answer}")

  // def main(args: Array[String]): Unit =
  //   Day07Part1
  //   Day07Part2
}
