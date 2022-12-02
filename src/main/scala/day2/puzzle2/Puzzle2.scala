package io.github.avapl
package day2.puzzle2

import day2.{Paper, Rock, Scissors, Shape}

object Puzzle2 extends App {
  val strategyGuide = Puzzle2InputParser.parsedInput
  val result = strategyGuide.foldLeft(0) { case (score, (opponentShape, roundResult)) =>
    val myShape = chooseShape(opponentShape, roundResult)
    score + myShape.score + roundResult.score
  }
  println(result)

  private def chooseShape(opponentShape: Shape, roundResult: RoundResult) =
    roundResult match {
      case Win =>
        opponentShape match {
          case Rock     => Paper
          case Paper    => Scissors
          case Scissors => Rock
        }
      case Lose =>
        opponentShape match {
          case Rock     => Scissors
          case Paper    => Rock
          case Scissors => Paper
        }
      case Draw => opponentShape
    }
}
