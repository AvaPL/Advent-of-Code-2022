package io.github.avapl
package day22.puzzle2

import day22._
import day22.puzzle2.FrameOfReference._

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
  val initialFrameOfReference = facingRight
  val sideSize = 50
  val (finalPosition, finalFrameOfReference) = movements.foldLeft((initialPosition, initialFrameOfReference)) {
    case ((position, frameOfReference), movement) => move(position, frameOfReference, movement)
  }
  val result = calculateScore(finalPosition, finalFrameOfReference.forward)
  println(result)

  @tailrec
  private def move(
      position: Position,
      frameOfReference: FrameOfReference,
      movement: Movement
  ): (Position, FrameOfReference) = {
    movement match {
      case rotation: Rotation => (position, frameOfReference.rotate(rotation))
      case Forward(0)         => (position, frameOfReference)
      case Forward(steps) =>
        val (newPosition, newFrameOfReference) = stepForward(position, frameOfReference)
        move(newPosition, newFrameOfReference, Forward(steps - 1))
    }
  }

  private def stepForward(position: Position, frameOfReference: FrameOfReference) = {
    val currentCubeSide = cubeSideFromPosition(position)
    val row = position.row + frameOfReference.forward.rowDirection
    val column = position.column + frameOfReference.forward.columnDirection
    val (newPosition, newFrameOfReference) =
      if (isSideWarp(row, column)) sideWarp(currentCubeSide, frameOfReference, row, column)
      else (Position(row, column), frameOfReference)
    if (board(newPosition.row)(newPosition.column) == openTile) (newPosition, newFrameOfReference)
    else (position, frameOfReference)
  }

  private def isSideWarp(row: Int, column: Int) = {
    for {
      row <- board.lift(row)
      rowElement <- row.lift(column)
      if rowElement != emptyTile
    } yield rowElement
  }.isEmpty

  private def sideWarp(currentCubeSide: CubeSide, frameOfReference: FrameOfReference, row: Int, column: Int) =
    (currentCubeSide, frameOfReference) match {
      case (One, `facingUp`)      => (Position(column + 2 * sideSize, 0), facingRight) // One to Six
      case (Six, `facingLeft`)    => (Position(0, row - 2 * sideSize), facingDown) // Six to One
      case (One, `facingLeft`)    => (Position(3 * sideSize - 1 - row, 0), facingRight) // One to Four
      case (Four, `facingLeft`)   => (Position(3 * sideSize - 1 - row, sideSize), facingRight) // Four to One
      case (Two, `facingDown`)    => (Position(column - sideSize, 2 * sideSize - 1), facingLeft) // Two to Three
      case (Three, `facingRight`) => (Position(sideSize - 1, row + sideSize), facingUp) // Three to Two
      case (Two, `facingRight`)   => (Position(3 * sideSize - 1 - row, 2 * sideSize - 1), facingLeft) // Two to Five
      case (Five, `facingRight`)  => (Position(3 * sideSize - 1 - row, 3 * sideSize - 1), facingLeft) // Five to Two
      case (Two, `facingUp`)      => (Position(4 * sideSize - 1, column - 2 * sideSize), facingUp) // Two to Six
      case (Six, `facingDown`)    => (Position(0, column + 2 * sideSize), facingDown) // Six to Two
      case (Three, `facingLeft`)  => (Position(2 * sideSize, row - sideSize), facingDown) // Three to Four
      case (Four, `facingUp`)     => (Position(column + sideSize, sideSize), facingRight) // Four to Three
      case (Five, `facingDown`)   => (Position(column + 2 * sideSize, sideSize - 1), facingLeft) // Five to Six
      case (Six, `facingRight`)   => (Position(3 * sideSize - 1, row - 2 * sideSize), facingUp) // Six to Five
    }

  private def cubeSideFromPosition(position: Position) =
    position match {
      case Position(row, column) if 0 <= row && row < sideSize                => if (column < 2 * sideSize) One else Two
      case Position(row, _) if sideSize <= row && row < 2 * sideSize          => Three
      case Position(row, column) if 2 * sideSize <= row && row < 3 * sideSize => if (column < sideSize) Four else Five
      case Position(row, _) if 3 * sideSize <= row && row < 4 * sideSize      => Six
    }
}
