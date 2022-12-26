package io.github.avapl
package day22.puzzle1

import day22._

import scala.annotation.tailrec
import scala.math.floorMod

object Puzzle1 extends App {
  val (board, movements) = PuzzleInputParser.parsedInput
  val initialPosition = Position(0, board.head.indexOf(openTile))
  val initialDirection: Direction = Right
  val (finalPosition, finalDirection) = movements.foldLeft((initialPosition, initialDirection)) {
    case ((position, direction), movement) => move(position, direction, movement)
  }
  val result = calculateScore(finalPosition, finalDirection)
  println(result)

  @tailrec
  private def move(position: Position, direction: Direction, movement: Movement): (Position, Direction) = {
    movement match {
      case rotation: Rotation => (position, direction.rotate(rotation))
      case Forward(0)         => (position, direction)
      case Forward(steps) =>
        val newPosition = stepForward(position, direction)
        move(newPosition, direction, Forward(steps - 1))
    }
  }

  private def stepForward(position: Position, direction: Direction) = {
    val row = newPositionRow(position, direction)
    val column = newPositionColumn(position, direction)
    if (board(row)(column) == openTile) Position(row, column)
    else position
  }

  private def newPositionRow(position: Position, direction: Direction) = {
    val firstRowTileIndex = board.indexWhere(isNonEmptyTile(position.column))
    val lastRowTileIndex = board.lastIndexWhere(isNonEmptyTile(position.column))
    floorMod(
      position.row + direction.rowDirection - firstRowTileIndex,
      lastRowTileIndex - firstRowTileIndex + 1
    ) + firstRowTileIndex
  }

  private def isNonEmptyTile(column: Int)(row: String) =
    column < row.length && row(column) != emptyTile

  private def newPositionColumn(position: Position, direction: Direction) = {
    val firstColumnTileIndex = board(position.row).indexWhere(isNonEmptyTile)
    val lastColumnTileIndex = board(position.row).lastIndexWhere(isNonEmptyTile)
    floorMod(
      position.column + direction.columnDirection - firstColumnTileIndex,
      lastColumnTileIndex - firstColumnTileIndex + 1
    ) + firstColumnTileIndex
  }

  private def isNonEmptyTile(rowElement: Char) =
    rowElement != emptyTile
}
