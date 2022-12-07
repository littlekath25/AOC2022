package AOC2022
import scala.io.Source
import scala.annotation.newMain

object Day07 {
  case class FileSystem(directories: List[Directory])
  case class Directory(name: String, files: List[File], directories: List[Directory])
  case class File(size: Int, name: String, extension: Option[String])

  val input = Source.fromResource("Example.txt").getLines.toList

  def createFile(name: String) : File =
    name match {
      case (s"$size ${name}.${extension}") => 
        File(size.toInt, name, Some(extension))
      case (s"$size $name") => 
        File(size.toInt, name, None)
      }
    
  def createDirectory(name: String): Directory =
    Directory(name, List.empty, List.empty)

  def overviewDirectory(current: Directory, input: List[String], filesystem: FileSystem) : FileSystem =
    input match {
      case head :: next => head match {
        case (s"dir $name") =>
          val updatedDir = current.copy(directories = current.directories ::: List(createDirectory(name)))
          overviewDirectory(updatedDir, next, filesystem.copy(directories = List(updatedDir)))
        case (s"$$ $instr") =>
          listFiles(current, input, filesystem)
        case x =>
            val updatedDir = current.copy(files = current.files ::: List(createFile(x)))
            overviewDirectory(updatedDir, next, filesystem)
      }
      case Nil => filesystem
    }

  def listFiles(current: Directory, input: List[String], filesystem: FileSystem) : FileSystem =
    input match {
        case head :: next => head match {
          case (s"$$ cd $name") => goToDirectory(name, next, filesystem)
          case (s"$$ ls") =>
            overviewDirectory(current, next, filesystem)
          case _ => filesystem
        }
        case Nil => ; filesystem
      }

  def goToDirectory(name: String, input: List[String], filesystem: FileSystem) : FileSystem =
    var currentDirectory = filesystem.directories.find(_.name == name)
    if (name == "/")
      if (filesystem.directories.isEmpty)
        val firstDirectory = List(Directory("/", List.empty, List.empty))
        goToDirectory("/", input, filesystem.copy(directories = filesystem.directories ::: firstDirectory))
      else
        currentDirectory = Some(filesystem.directories.head)
        listFiles(currentDirectory.get, input, filesystem)
    else
      listFiles(currentDirectory.get, input, filesystem)

  def iterateThroughFiles(input: List[String], filesystem: FileSystem): FileSystem =
    input match {
      case head :: next => head match {
        case (s"$$ cd $to") =>
          val newsystem = goToDirectory(to, next, filesystem)
          newsystem
      }
      case Nil => filesystem
    }

  def Day07Part1 =
    val answer = iterateThroughFiles(input, FileSystem(List.empty))

    println(s"Day 07 - part 1: $answer")

  def Day07Part2 =
    val answer = ???

    println(s"Day 07 - part 2: ${answer}")

  def main(args: Array[String]): Unit =
    Day07Part1
  //   Day07Part2
}
