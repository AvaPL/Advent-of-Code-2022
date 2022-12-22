package io.github.avapl

import scala.annotation.tailrec
import scala.math.floorMod

package object day20 {

  def mixNumbers(numbers: IndexedSeq[Long], n: Int): IndexedSeq[Long] = {
    type Id = Int
    val numbersWithId = numbers.zipWithIndex

    @tailrec
    def loop(numbersToMix: IndexedSeq[(Long, Id)], n: Int): IndexedSeq[Long] =
      if (n <= 0)
        numbersToMix.map { case (number, _) => number }
      else {
        val mixedNumbers = numbersWithId.foldLeft(numbersToMix) { case (mixedNumbers, (number, id)) =>
          val numberIndex = mixedNumbers.indexWhere { case (_, mixedNumberId) => mixedNumberId == id }
          val withElementRemoved = mixedNumbers.removeAt(numberIndex)
          val newNumberIndex = floorMod(numberIndex + number, withElementRemoved.length).toInt
          withElementRemoved.insertAt(newNumberIndex, (number, id))
        }
        loop(mixedNumbers, n - 1)
      }

    loop(numbersWithId, n)
  }

  implicit class Patch[T](indexedSeq: IndexedSeq[T]) {
    def removeAt(index: Int): IndexedSeq[T] = indexedSeq.patch(index, Nil, 1)
    def insertAt(index: Int, value: T): IndexedSeq[T] = indexedSeq.patch(index, Seq(value), 0)
  }

  implicit class CircularAt[T](indexedSeq: IndexedSeq[T]) {
    def circularAt(index: Int): T = indexedSeq(floorMod(index, indexedSeq.length))
  }
}
