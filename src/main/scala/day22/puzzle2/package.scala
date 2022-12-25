package io.github.avapl
package day22

import scala.util.chaining._

package object puzzle2 {

  sealed trait CubeSide
  case object One extends CubeSide
  case object Two extends CubeSide
  case object Three extends CubeSide
  case object Four extends CubeSide
  case object Five extends CubeSide
  case object Six extends CubeSide

  sealed abstract class Direction(val rowDirection: Int, val columnDirection: Int)
  case object Up extends Direction(-1, 0)
  case object Down extends Direction(1, 0)
  case object Left extends Direction(0, -1)
  case object Right extends Direction(0, 1)

  case class FrameOfReference private (forward: Direction, right: Direction) {

    def rotate(rotation: Rotation): FrameOfReference = {
      (forward, right, rotation) match {
        case (Up, Right, Clockwise)          => (Right, Down)
        case (Up, Right, CounterClockwise)   => (Left, Up)
        case (Right, Down, Clockwise)        => (Down, Left)
        case (Right, Down, CounterClockwise) => (Up, Right)
        case (Down, Left, Clockwise)         => (Left, Up)
        case (Down, Left, CounterClockwise)  => (Right, Down)
        case (Left, Up, Clockwise)           => (Up, Right)
        case (Left, Up, CounterClockwise)    => (Down, Left)
      }
    }.pipe { case (forward, right) => FrameOfReference(forward, right) }
  }

  object FrameOfReference {
    val facingUp: FrameOfReference = FrameOfReference(Up, Right)
    val facingRight: FrameOfReference = facingUp.rotate(Clockwise)
    val facingDown: FrameOfReference = facingRight.rotate(Clockwise)
    val facingLeft: FrameOfReference = facingDown.rotate(Clockwise)
  }
}
