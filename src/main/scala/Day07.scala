// package AOC2022
// import scala.io.Source
// import scala.annotation.newMain

// object Day07 {
//   case class FileSystem(directories: List[Directory])
//   case class Directory(name: String, files: List[File], directories: List[Directory])
//   case class File(size: Int, name: String, extension: Option[String])

//   val input = Source.fromResource("Example.txt").getLines.toList

//   def createFile(name: String) : File =
//     name match {
//       case (s"$size ${name}.${extension}") => 
//         File(size.toInt, name, Some(extension))
//       case (s"$size $name") => 
//         File(size.toInt, name, None)
//       }
    
//   def createDirectory(name: String): Directory =
//     Directory(name, List.empty, List.empty)

//   def overviewDirectory(current: Directory, input: List[String], filesystem: FileSystem) : FileSystem =
//     input match {
//       case head :: next => head match {
//         case (s"$$ $instr") =>
//           listFiles(current, input, filesystem)
//         case (s"dir $name") =>
//           val updatedDir = current.copy(directories = current.directories ::: List(createDirectory(name)))
//           overviewDirectory(updatedDir, next, filesystem.copy(directories = List(updatedDir)))
//         case x =>
//           val updatedDir = current.copy(files = current.files ::: List(createFile(x)))
//           overviewDirectory(updatedDir, next, filesystem)
//       }
//       case Nil => filesystem
//     }

//   def listFiles(current: Directory, input: List[String], filesystem: FileSystem) : FileSystem =
//     input match {
//         case head :: next => head match {
//           case (s"$$ cd $name") => println(s"TRYINT TO GO TO: $name from ${current.name}"); goToDirectory(name, next, filesystem, current.name)
//           case (s"$$ ls") =>
//             overviewDirectory(current, next, filesystem)
//           case _ => filesystem
//         }
//         case Nil => filesystem
//       }

//   def getCorrectDirectory(current: String, to: String, filesystem: FileSystem) : Directory =
//     val found = filesystem.directories.find(_.name == current).get.directories.find(_.name == to).get
//     found

//   def goToDirectory(to: String, input: List[String], filesystem: FileSystem, current: String) : FileSystem =
//     if (to == "/")
//       listFiles(filesystem.directories.head, input, filesystem)
//     else
//       val currentDirectory = getCorrectDirectory(current, to, filesystem)
//       listFiles(currentDirectory, input, filesystem)

//   def iterateThroughFiles(input: List[String], filesystem: FileSystem, current: String): FileSystem =
//     input match {
//       case head :: next => head match {
//         case (s"$$ cd $to") =>
//           val newsystem = goToDirectory(to, next, filesystem, current)
//           newsystem
//       }
//       case Nil => filesystem
//     }
      
//   def Day07Part1 =
//     val answer = iterateThroughFiles(input, 
//       FileSystem(
//         List(
//           Directory("/", List.empty, List.empty)
//           )), "/")

//     println(s"Day 07 - part 1: $answer")

//   def Day07Part2 =
//     val answer = ???

//     println(s"Day 07 - part 2: ${answer}")

//   def main(args: Array[String]): Unit =
//     Day07Part1
//   //   Day07Part2
// }
