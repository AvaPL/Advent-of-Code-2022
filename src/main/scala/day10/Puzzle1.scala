package io.github.avapl
package day10

object Puzzle1 extends App {
  val instructions = PuzzleInputParser.parsedInput
  val cycleToRegisterX = registerXValues(instructions)
  val result = (20 to 220 by 40).map(getSignalStrength).sum
  println(result)

  private def getSignalStrength(cycle: Int) = {
    val duringCycleRegisterX = cycleToRegisterX(cycle - 1)
    cycle * duringCycleRegisterX
  }
}
