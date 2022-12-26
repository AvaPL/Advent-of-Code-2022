package io.github.avapl

package object day22 {
  type Board = Seq[String]

  sealed trait Movement

  case class Forward(steps: Int) extends Movement

  sealed trait Rotation extends Movement
  case object CounterClockwise extends Rotation
  case object Clockwise extends Rotation

  val emptyTile = ' '
  val openTile = '.'

  case class Position(row: Int, column: Int)

  sealed abstract class Direction(val rowDirection: Int, val columnDirection: Int) {
    def rotate(rotation: Rotation): Direction =
      (this, rotation) match {
        case (Up, Clockwise)           => Right
        case (Up, CounterClockwise)    => Left
        case (Down, Clockwise)         => Left
        case (Down, CounterClockwise)  => Right
        case (Left, Clockwise)         => Up
        case (Left, CounterClockwise)  => Down
        case (Right, Clockwise)        => Down
        case (Right, CounterClockwise) => Up
      }
  }
  case object Up extends Direction(-1, 0)
  case object Down extends Direction(1, 0)
  case object Left extends Direction(0, -1)
  case object Right extends Direction(0, 1)

  def calculateScore(position: Position, direction: Direction): Int =
    1000 * (position.row + 1) + 4 * (position.column + 1) + {
      direction match {
        case Right => 0
        case Down  => 1
        case Left  => 2
        case Up    => 3
      }
    }
}
