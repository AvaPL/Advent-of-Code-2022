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
  val initialPosition = Position(0, board.head.indexOf('.'))
  val initialFrameOfReference = FrameOfReference.facingRight
  val (finalPosition, finalFrameOfReference) = movements.foldLeft((initialPosition, initialFrameOfReference)) {
    case ((position, frameOfReference), movement) => move(position, frameOfReference, movement)
  }
  val result = 1000 * (finalPosition.row + 1) + 4 * (finalPosition.column + 1) + {
    finalFrameOfReference.forward match {
      case Right => 0
      case Down  => 1
      case Left  => 2
      case Up    => 3
    }
  }
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
    val cubeSide = cubeSideFromPosition(position)
    val row = position.row + frameOfReference.forward.rowDirection
    val column = position.column + frameOfReference.forward.columnDirection
    val isSideWarp = {
      for {
        row <- board.lift(row)
        rowElement <- row.lift(column)
        if isNonEmptyTile(rowElement)
      } yield rowElement
    }.isEmpty
    val (newPosition, newFrameOfReference) = if (isSideWarp) {
      cubeSide match {
        case One if frameOfReference == FrameOfReference.facingUp => // One to Six
          (Position(column - 50 + 150, 0), FrameOfReference.facingRight)
        case One if frameOfReference == FrameOfReference.facingLeft => // One to Four
          (Position((49 - row) + 100, 0), FrameOfReference.facingRight)
        case Two if frameOfReference == FrameOfReference.facingUp => // Two to Six
          (Position(199, column - 100), FrameOfReference.facingUp)
        case Two if frameOfReference == FrameOfReference.facingRight => // Two to Five
          (Position((49 - row) + 100, 99), FrameOfReference.facingLeft)
        case Two if frameOfReference == FrameOfReference.facingDown => // Two to Three
          (Position(column - 100 + 50, 99), FrameOfReference.facingLeft)
        case Three if frameOfReference == FrameOfReference.facingRight => // Three to Two
          (Position(49, row - 50 + 100), FrameOfReference.facingUp)
        case Three if frameOfReference == FrameOfReference.facingLeft => // Three to Four
          (Position(100, row - 50), FrameOfReference.facingDown)
        case Four if frameOfReference == FrameOfReference.facingUp => // Four to Three
          (Position(column + 50, 50), FrameOfReference.facingRight)
        case Four if frameOfReference == FrameOfReference.facingLeft => // Four to One
          (Position(49 - (row - 100), 50), FrameOfReference.facingRight)
        case Five if frameOfReference == FrameOfReference.facingRight => // Five to Two
          (Position(49 - (row - 100), 149), FrameOfReference.facingLeft)
        case Five if frameOfReference == FrameOfReference.facingDown => // Five to Six
          (Position(column - 50 + 150, 49), FrameOfReference.facingLeft)
        case Six if frameOfReference == FrameOfReference.facingRight => // Six to Five
          (Position(149, row - 150 + 50), FrameOfReference.facingUp)
        case Six if frameOfReference == FrameOfReference.facingDown => // Six to Two
          (Position(0, column + 100), FrameOfReference.facingDown)
        case Six if frameOfReference == FrameOfReference.facingLeft => // Six to One
          (Position(0, row - 150 + 50), FrameOfReference.facingDown)
      }
    } else (Position(row, column), frameOfReference)
    if (board(newPosition.row)(newPosition.column) == '.') (newPosition, newFrameOfReference)
    else (position, frameOfReference)
  }

  private def isNonEmptyTile(rowElement: Char) =
    rowElement != ' '

  private def cubeSideFromPosition(position: Position) =
    position match {
      case Position(row, column) if 0 <= row && row < 50    => if (column < 100) One else Two
      case Position(row, _) if 50 <= row && row < 100       => Three
      case Position(row, column) if 100 <= row && row < 150 => if (column < 50) Four else Five
      case Position(row, _) if 150 <= row && row < 200      => Six
    }
}
