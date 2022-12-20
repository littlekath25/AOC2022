package AOC2022
import scala.io.Source
import scala.collection.mutable._

object Day20 {
  type Original = Int
  type NewIndex = Int
  type Value = Int

  val original = Source.fromResource("Example.txt").getLines.zipWithIndex.toList.map(x => (x._2 + 1 -> x._1.toInt))
  val sorted = original.map{case(k,v) => k -> (k, v)}.to(scala.collection.mutable.Map)

  def calculateIndex(toShift: Int): Int =
    val modulod = toShift % original.size
    if (modulod == 0)
      original.size
    else if (modulod < 0)
      (original.size + modulod) - 1
    else
      toShift % original.size

  def shiftOneBackward(shiftMap: Map[Original, (NewIndex, Value)], original: Int, curIdxToShiftFrom: Int, idxToShiftTo: Int): Map[Original, (NewIndex, Value)] =
    val newShiftedMap = shiftMap.map { pair =>
      if (pair._1 == original)
        (pair._1 -> (idxToShiftTo, pair._2._2))
      else if (pair._2._1 <= idxToShiftTo && pair._2._1 > curIdxToShiftFrom)
        (pair._1 -> (pair._2._1 - 1, pair._2._2))
      else if (pair._2._1 > idxToShiftTo && pair._2._1 <= curIdxToShiftFrom)
        (pair._1 -> (pair._2._1 + 1, pair._2._2))
      else
        pair
    }
    // println(s"THE SHIFTED ONE: $newShiftedMap")
    newShiftedMap

  def shift(shiftMap: Map[Original, (NewIndex, Value)], original: Original, curIdx: Int, newIdx: Int): Map[Original, (NewIndex, Value)] = 
    val calculatedIdx = calculateIndex(newIdx)
    // println(s"NEW IDX: $calculatedIdx")
    shiftOneBackward(shiftMap, original, curIdx, calculatedIdx)

  def sortingAlgorithm(sorted: Map[Original, (NewIndex, Value)], idx: Int = 1): Map[Original, (NewIndex, Value)] =
    var newSorted = sorted
    if (idx > original.size - 1)
      println(s"THE SORTED LIST: ${newSorted.toSeq.sortBy(x => x._2._1).map(_._2._2)}")
      sorted
    else
      val value = sorted(idx)
      val newIdx = (value._1 + value._2)
      // println(s"THTE VALUE WE WANT TO MOVE IS: ${idx} ${sorted(idx)}")
      newSorted = shift(sorted, idx, value._1, newIdx)
      // println(s"THE SORTED LIST: ${newSorted.toSeq.sortBy(x => x._2._1).map(_._2._2)}")
      println(s"THE SORTED LIST: ${newSorted.toSeq.sortBy(x => x._2._1).map(_._2._2)}")
      sortingAlgorithm(newSorted, idx + 1)

  def Day20Part1 =
    val answer = sortingAlgorithm(sorted).toSeq.sortBy(x => x._2._1).map(_._2._2)
    val idxOfZero = answer.indexOf(0)

    // println(s"$answer")
    println(s"GET NUMBER: ${answer((idxOfZero + 1000) % original.size)} ${answer((idxOfZero + 2000) % original.size)} ${answer((idxOfZero + 3000) % original.size)}")
    // println(s"Day 20 - part 1: ${answer}")

  def Day20Part2 =
    val answer = ???

    println(s"Day 20 - part 2: ${answer}")

  def main(args: Array[String]): Unit =
    Day20Part1
  //   Day20Part2
}