package io.github.avapl
package day2.puzzle1

object Puzzle1 extends App {
  val strategyGuide = Puzzle1InputParser.parsedInput
  val result = strategyGuide.foldLeft(0) {
    case (score, (opponentShape, myShape)) if myShape.beats(opponentShape) => score + myShape.score + 6 // win
    case (score, (opponentShape, myShape)) if opponentShape == myShape     => score + myShape.score + 3 // draw
    case (score, (_, myShape))                                             => score + myShape.score // lose
  }
  println(result)
}
