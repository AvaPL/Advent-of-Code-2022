package io.github.avapl
package day10

import scala.math.abs

object Puzzle2 extends App {
  val instructions = PuzzleInputParser.parsedInput
  val cycleToRegisterX = registerXValues(instructions)
  val crtWidth = 40
  val crtHeight = 6
  val result = (0 until crtWidth * crtHeight)
    .map { cycle =>
      val spriteIndex = cycleToRegisterX(cycle)
      if (isPixelLit(cycle, spriteIndex)) '#' else '.'
    }
    .mkString
    .grouped(crtWidth)
    .mkString("\n")
  println(result)

  private def isPixelLit(cycle: Int, spriteIndex: Int) =
    abs(cycle % crtWidth - spriteIndex) <= 1
}
