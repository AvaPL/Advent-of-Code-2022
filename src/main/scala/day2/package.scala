package io.github.avapl

package object day2 {

  sealed abstract class Shape(val score: Int)
  case object Rock extends Shape(score = 1)
  case object Paper extends Shape(score = 2)
  case object Scissors extends Shape(score = 3)
}
