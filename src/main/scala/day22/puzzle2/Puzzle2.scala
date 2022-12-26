package io.github.avapl
package day22.puzzle2

import day22._

import scala.annotation.tailrec

/** Assumes the following layout:
  *
  * {{{
  *     #########
  *     # 1 # 2 #
  *     #########
  *     # 3 #
  * #########
  * # 4 # 5 #
  * #########
  * # 6 #
  * #####
  * }}}
  */
object Puzzle2 extends App {
  val (board, movements) = PuzzleInputParser.parsedInput
  val initialPosition = Position(0, board.head.indexOf(openTile))
  val initialDirection: Direction = Right
  val sideSize = 50
  val (finalPosition, finalDirection) = movements.foldLeft((initialPosition, initialDirection)) {
    case ((position, frameOfReference), movement) => move(position, frameOfReference, movement)
  }
  val result = calculateScore(finalPosition, finalDirection)
  println(result)

  @tailrec
  private def move(
      position: Position,
      direction: Direction,
      movement: Movement
  ): (Position, Direction) = {
    movement match {
      case rotation: Rotation => (position, direction.rotate(rotation))
      case Forward(0)         => (position, direction)
      case Forward(steps) =>
        val (newPosition, newDirection) = stepForward(position, direction)
        move(newPosition, newDirection, Forward(steps - 1))
    }
  }

  private def stepForward(position: Position, direction: Direction) = {
    val currentCubeSide = cubeSideFromPosition(position)
    val row = position.row + direction.rowDirection
    val column = position.column + direction.columnDirection
    val (newPosition, newFrameOfReference) =
      if (isSideWarp(row, column)) sideWarp(currentCubeSide, direction, row, column)
      else (Position(row, column), direction)
    if (board(newPosition.row)(newPosition.column) == openTile) (newPosition, newFrameOfReference)
    else (position, direction)
  }

  private def cubeSideFromPosition(position: Position) =
    position match {
      case Position(row, column) if 0 <= row && row < sideSize                => if (column < 2 * sideSize) One else Two
      case Position(row, _) if sideSize <= row && row < 2 * sideSize          => Three
      case Position(row, column) if 2 * sideSize <= row && row < 3 * sideSize => if (column < sideSize) Four else Five
      case Position(row, _) if 3 * sideSize <= row && row < 4 * sideSize      => Six
    }

  private def isSideWarp(row: Int, column: Int) = {
    for {
      row <- board.lift(row)
      rowElement <- row.lift(column)
      if rowElement != emptyTile
    } yield rowElement
  }.isEmpty

  private def sideWarp(currentCubeSide: CubeSide, direction: Direction, row: Int, column: Int) =
    (currentCubeSide, direction) match {
      case (One, Up)      => (Position(column + 2 * sideSize, 0), Right) // One to Six
      case (Six, Left)    => (Position(0, row - 2 * sideSize), Down) // Six to One
      case (One, Left)    => (Position(3 * sideSize - 1 - row, 0), Right) // One to Four
      case (Four, Left)   => (Position(3 * sideSize - 1 - row, sideSize), Right) // Four to One
      case (Two, Down)    => (Position(column - sideSize, 2 * sideSize - 1), Left) // Two to Three
      case (Three, Right) => (Position(sideSize - 1, row + sideSize), Up) // Three to Two
      case (Two, Right)   => (Position(3 * sideSize - 1 - row, 2 * sideSize - 1), Left) // Two to Five
      case (Five, Right)  => (Position(3 * sideSize - 1 - row, 3 * sideSize - 1), Left) // Five to Two
      case (Two, Up)      => (Position(4 * sideSize - 1, column - 2 * sideSize), Up) // Two to Six
      case (Six, Down)    => (Position(0, column + 2 * sideSize), Down) // Six to Two
      case (Three, Left)  => (Position(2 * sideSize, row - sideSize), Down) // Three to Four
      case (Four, Up)     => (Position(column + sideSize, sideSize), Right) // Four to Three
      case (Five, Down)   => (Position(column + 2 * sideSize, sideSize - 1), Left) // Five to Six
      case (Six, Right)   => (Position(3 * sideSize - 1, row - 2 * sideSize), Up) // Six to Five
    }
}
