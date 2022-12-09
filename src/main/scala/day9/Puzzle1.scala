package io.github.avapl
package day9

object Puzzle1 extends App {
  val motions = PuzzleInputParser.parsedInput
  val rope = Seq.fill(2)((0, 0))
  val result = countUniqueTailPositions(rope, motions)
  println(result)
}
