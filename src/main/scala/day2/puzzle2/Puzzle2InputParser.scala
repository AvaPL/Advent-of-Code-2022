package io.github.avapl
package day2.puzzle2

import day2._
import util.InputParser
import util.InputParser._

object Puzzle2InputParser extends InputParser[StrategyGuide](day = 2) {

  override protected def parse(string: String): StrategyGuide =
    for {
      line <- string.splitLines
      Seq(opponentShapeString, roundResultString) = line.splitBy(" ")
    } yield (parseShape(opponentShapeString), parseRoundResult(roundResultString))

  private def parseShape(shapeString: String) =
    shapeString match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }

  private def parseRoundResult(roundResultString: String) =
    roundResultString match {
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
    }
}
