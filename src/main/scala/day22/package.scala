package io.github.avapl

package object day22 {
  type Board = Seq[String]

  sealed trait Movement

  case class Forward(steps: Int) extends Movement

  sealed trait Rotation extends Movement
  case object CounterClockwise extends Rotation
  case object Clockwise extends Rotation

  case class Position(row: Int, column: Int)
}
