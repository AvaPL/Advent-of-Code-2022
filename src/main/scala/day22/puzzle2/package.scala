package io.github.avapl
package day22

package object puzzle2 {

  case class FrameOfReference private (forward: Direction, right: Direction) {

    import FrameOfReference._

    def rotate(rotation: Rotation): FrameOfReference =
      (this, rotation) match {
        case (`facingUp`, Clockwise)           => facingRight
        case (`facingUp`, CounterClockwise)    => facingLeft
        case (`facingRight`, Clockwise)        => facingDown
        case (`facingRight`, CounterClockwise) => facingUp
        case (`facingDown`, Clockwise)         => facingLeft
        case (`facingDown`, CounterClockwise)  => facingRight
        case (`facingLeft`, Clockwise)         => facingUp
        case (`facingLeft`, CounterClockwise)  => facingDown
      }
  }

  object FrameOfReference {
    val facingUp: FrameOfReference = FrameOfReference(Up, Right)
    val facingRight: FrameOfReference = FrameOfReference(Right, Down)
    val facingDown: FrameOfReference = FrameOfReference(Down, Left)
    val facingLeft: FrameOfReference = FrameOfReference(Left, Up)
  }

  sealed trait CubeSide
  case object One extends CubeSide
  case object Two extends CubeSide
  case object Three extends CubeSide
  case object Four extends CubeSide
  case object Five extends CubeSide
  case object Six extends CubeSide
}
