package io.github.avapl
package day20

object Puzzle1 extends App {
  val numbers = PuzzleInputParser.parsedInput
  val mixedNumbers = mixNumbers(numbers, n = 1)
  val indexOf0 = mixedNumbers.indexOf(0)
  val result =
    mixedNumbers.circularAt(indexOf0 + 1000) +
      mixedNumbers.circularAt(indexOf0 + 2000) +
      mixedNumbers.circularAt(indexOf0 + 3000)
  println(result)
}
