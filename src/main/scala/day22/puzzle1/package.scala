package io.github.avapl
package day22

package object puzzle1 {

  implicit class Rotate(direction: Direction) {
    def rotate(rotation: Rotation): Direction =
      (direction, rotation) match {
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
}
