package io.github.avapl
package day24

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[(Int, Int, Map[Position, Seq[Blizzard]])](day = 24) {

  override protected def parse(string: String): (Int, Int, Map[Position, Seq[Blizzard]]) = {
    val valleyWithoutBorders = string.splitLines.tail.init.map(_.tail.init)
    val blizzards = parseBlizzards(valleyWithoutBorders)
    val valleyWidth = valleyWithoutBorders.head.length
    val valleyHeight = valleyWithoutBorders.length
    (valleyWidth, valleyHeight, blizzards)
  }

  private def parseBlizzards(valley: Seq[String]) = {
    for {
      row <- valley.indices
      column <- valley.head.indices
      symbol = valley(row)(column)
      if symbol != '.'
    } yield {
      val (rowDirection, columnDirection) = parseRowColumnDirection(symbol)
      Position(row, column) -> Blizzard(rowDirection, columnDirection)
    }
  }.groupMap { case (position, _) => position } { case (_, blizzard) => blizzard }

  private def parseRowColumnDirection(symbol: Char) =
    symbol match {
      case '^' => (-1, 0)
      case 'v' => (1, 0)
      case '<' => (0, -1)
      case '>' => (0, 1)
    }
}
