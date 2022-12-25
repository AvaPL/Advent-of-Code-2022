package io.github.avapl

package object day22 {
  type Board = Seq[String]

  sealed trait Movement

  case class Forward(steps: Int) extends Movement

  sealed trait Rotation extends Movement
  case object CounterClockwise extends Rotation
  case object Clockwise extends Rotation

  case class Position(row: Int, column: Int)

  sealed abstract class Direction(val rowDirection: Int, val columnDirection: Int)
  case object Up extends Direction(-1, 0)
  case object Down extends Direction(1, 0)
  case object Left extends Direction(0, -1)
  case object Right extends Direction(0, 1)
}
