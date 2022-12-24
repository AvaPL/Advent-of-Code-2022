package io.github.avapl
package day24

object Puzzle2 extends App {
  val (valleyWidth, valleyHeight, blizzards) = PuzzleInputParser.parsedInput
  val valley = new Valley(valleyWidth, valleyHeight, blizzards)
  val startToGoal = valley.findShortestPath(
    startingPosition = valley.entrancePosition,
    targetPosition = valley.exitPosition,
    startingMinute = 0
  )
  val goalToStart = valley.findShortestPath(
    startingPosition = valley.exitPosition,
    targetPosition = valley.entrancePosition,
    startingMinute = startToGoal
  )
  val result = valley.findShortestPath(
    startingPosition = valley.entrancePosition,
    targetPosition = valley.exitPosition,
    startingMinute = goalToStart
  )
  println(result)
}
