package io.github.avapl
package day23

object Puzzle2 extends App {
  val elfPositions = PuzzleInputParser.parsedInput
  val lastRoundWithMovement = LazyList
    .unfold((elfPositions, 0)) { case (elfPositions, round) =>
      val positionsAfterRound = executeRound(elfPositions, round)
      Option.when(positionsAfterRound != elfPositions)(round + 1, (positionsAfterRound, round + 1))
    }
    .last
  val result = lastRoundWithMovement + 1
  println(result)
}
