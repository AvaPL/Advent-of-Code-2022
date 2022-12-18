package io.github.avapl
package day18

object Puzzle1 extends App {
  val cubePositions = PuzzleInputParser.parsedInput
  val result = calculateVisibleSides(cubePositions)
  println(result)
}
