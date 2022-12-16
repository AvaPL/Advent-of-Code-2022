package io.github.avapl
package day14

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[CaveSystem](day = 14) {

  override protected def parse(string: String): CaveSystem = {
    val lineEndsByInputLine = string.splitLines.map(parseLineEnds)
    val allLineEnds = lineEndsByInputLine.flatten
    val leftMargin = allLineEnds.map { case (_, column) => column }.min - 1
    val rightMargin = allLineEnds.map { case (_, column) => column }.max
    val caveSystemWidth = rightMargin - leftMargin + 3
    val caveSystemHeight = allLineEnds.map { case (row, _) => row }.max + 2
    buildCaveSystem(lineEndsByInputLine, caveSystemWidth, caveSystemHeight, leftMargin)
  }

  private def parseLineEnds(line: String) =
    line.splitBy(" -> ").map { case s"$column,$row" =>
      (row.toInt, column.toInt)
    }

  private def buildCaveSystem(
      lineEndsByInputLine: Seq[Seq[(Int, Int)]],
      caveSystemWidth: Int,
      caveSystemHeight: Int,
      leftMargin: Int
  ) = {
    val caveSystem = Array.fill(caveSystemHeight)(Array.fill(caveSystemWidth)(emptySpace))
    for {
      inputLineLineEnds <- lineEndsByInputLine
      ((startRow, startColumn), (endRow, endColumn)) <- startsAndEnds(inputLineLineEnds, leftMargin)
      (row, column) <- rowColumns(startRow, startColumn)(endRow, endColumn)
    } caveSystem(row)(column) = rock
    caveSystem(0)(500 - leftMargin) = sand
    caveSystem.map(_.toIndexedSeq).toIndexedSeq
  }

  private def startsAndEnds(lineEnds: Seq[(Int, Int)], leftMargin: Int) =
    lineEnds.sliding(2).map { case Seq((startRow, startColumn), (endRow, endColumn)) =>
      ((startRow, startColumn - leftMargin), (endRow, endColumn - leftMargin))
    }

  private def rowColumns(startRow: Int, startColumn: Int)(endRow: Int, endColumn: Int) =
    for {
      row <- startRow.min(endRow) to startRow.max(endRow)
      column <- startColumn.min(endColumn) to startColumn.max(endColumn)
    } yield (row, column)
}
