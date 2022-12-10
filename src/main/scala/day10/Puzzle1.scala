package io.github.avapl
package day10

object Puzzle1 extends App {
  val instructions = PuzzleInputParser.parsedInput
  val afterCycleToRegisterX = mapCycleToRegisterX(instructions)
  val result = (20 to 220 by 40).map(getSignalStrength).sum
  println(result)

  private def getSignalStrength(cycle: Int) = {
    val duringCycleRegisterX = afterCycleToRegisterX.getOrElse(cycle - 1, afterCycleToRegisterX(cycle - 2))
    cycle * duringCycleRegisterX
  }
}
