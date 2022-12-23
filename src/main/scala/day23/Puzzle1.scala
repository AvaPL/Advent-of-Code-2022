package io.github.avapl
package day23

import scala.util.chaining._

object Puzzle1 extends App {
  val elfPositions = PuzzleInputParser.parsedInput
  val finalPositions = (0 until 10).foldLeft(elfPositions) { case (elfPositions, round) =>
    executeRound(elfPositions, round)
  }
  val result = enclosingRectangleArea - elfPositions.size
  println(result)

  private lazy val enclosingRectangleArea =
    finalPositions
      .foldLeft((0, 0, 0, 0)) { case ((minRow, maxRow, minColumn, maxColumn), Position(row, column)) =>
        (minRow.min(row), maxRow.max(row), minColumn.min(column), maxColumn.max(column))
      }
      .pipe { case (minRow, maxRow, minColumn, maxColumn) =>
        (maxRow - minRow + 1) * (maxColumn - minColumn + 1)
      }
}
