package io.github.avapl
package day2.puzzle1

import day2.{Paper, Rock, Scissors}
import util.InputParser
import util.InputParser._

object Puzzle1InputParser extends InputParser[StrategyGuide](day = 2) {

  override protected def parse(string: String): StrategyGuide =
    for {
      line <- string.splitLines
      Seq(opponentShapeString, myShapeString) = line.splitBy(" ")
    } yield (parseShape(opponentShapeString), parseShape(myShapeString))

  private def parseShape(shapeString: String) =
    shapeString match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
    }
}
