package io.github.avapl
package day6

object Puzzle2 extends App {
  val signal = PuzzleInputParser.parsedInput
  var startOfMessageMarker = 13
  signal.sliding(14).find { fourCharacters =>
    startOfMessageMarker += 1
    fourCharacters.distinct.length == 14
  }
  println(startOfMessageMarker)
}
