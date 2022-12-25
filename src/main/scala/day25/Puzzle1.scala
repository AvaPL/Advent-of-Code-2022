package io.github.avapl
package day25

object Puzzle1 extends App {
  val snafuNumbers = PuzzleInputParser.parsedInput
  val decodedSnafuNumbers = snafuNumbers.map(SnafuCoder.decode)
  val result = SnafuCoder.encode(decodedSnafuNumbers.sum)
  println(result)
}
