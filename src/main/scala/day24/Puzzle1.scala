package io.github.avapl
package day24

object Puzzle1 extends App {
  val (valleyWidth, valleyHeight, blizzards) = PuzzleInputParser.parsedInput
  val valley = new Valley(valleyWidth, valleyHeight, blizzards)
  val result = valley.findShortestPath(
    startingPosition = valley.entrancePosition,
    targetPosition = valley.exitPosition,
    startingMinute = 0
  )
  println(result)
}
