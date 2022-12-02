package io.github.avapl

package object day2 {

  sealed abstract class Shape(val score: Int) {
    def beats(shape: Shape): Boolean
  }

  case object Rock extends Shape(score = 1) {
    override def beats(shape: Shape): Boolean =
      shape == Scissors
  }

  case object Paper extends Shape(score = 2) {
    override def beats(shape: Shape): Boolean =
      shape == Rock
  }

  case object Scissors extends Shape(score = 3) {
    override def beats(shape: Shape): Boolean =
      shape == Paper
  }
}
