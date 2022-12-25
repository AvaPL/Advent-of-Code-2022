package io.github.avapl
package day22

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[(Board, Seq[Movement])](day = 22) {

  override protected def parse(string: String): (Board, Seq[Movement]) = {
    val Seq(boardString, movementsString) = string.splitBlocks
    val movements = for {
      movement <- movementsString.splitByRegex("(?=[RL])|(?<=[RL])")
    } yield movement match {
      case "R"   => Clockwise
      case "L"   => CounterClockwise
      case steps => Forward(steps.toInt)
    }
    (boardString.splitLines, movements)
  }
}
