package io.github.avapl
package day2.puzzle1

import day2.{Paper, Rock, Scissors, Shape}

object Puzzle1 extends App {
  val strategyGuide = Puzzle1InputParser.parsedInput
  val result = strategyGuide.foldLeft(0) {
    case (score, (opponentShape, myShape)) if myShape.beats(opponentShape) => score + myShape.score + 6
    case (score, (opponentShape, myShape)) if opponentShape == myShape     => score + myShape.score + 3
    case (score, (_, myShape))                                             => score + myShape.score
  }
  println(result)

  implicit class Beats(shape: Shape) {
    def beats(other: Shape): Boolean = shape match {
      case Rock     => other == Scissors
      case Paper    => other == Rock
      case Scissors => other == Paper
    }
  }
}
