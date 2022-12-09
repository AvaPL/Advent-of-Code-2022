package io.github.avapl
package day9

object Puzzle2 extends App {
  val motions = PuzzleInputParser.parsedInput
  val rope = Seq.fill(10)((0, 0))
  val result = countUniqueTailPositions(rope, motions)
  println(result)
}
