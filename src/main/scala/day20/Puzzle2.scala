package io.github.avapl
package day20

object Puzzle2 extends App {
  val numbers = PuzzleInputParser.parsedInput
  val decryptionKey = 811589153
  val numbersWithDecryptionKey = numbers.map(_ * decryptionKey)
  val mixedNumbers = mixNumbers(numbersWithDecryptionKey, n = 10)
  val indexOf0 = mixedNumbers.indexOf(0)
  val result =
    mixedNumbers.circularAt(indexOf0 + 1000) +
      mixedNumbers.circularAt(indexOf0 + 2000) +
      mixedNumbers.circularAt(indexOf0 + 3000)
  println(result)
}
