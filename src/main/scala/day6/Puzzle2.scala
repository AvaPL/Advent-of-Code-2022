package io.github.avapl
package day6

object Puzzle2 extends App {
  val signal = PuzzleInputParser.parsedInput
  val distinctCharactersCount = 14
  var startOfMessageMarker = distinctCharactersCount - 1
  signal.sliding(distinctCharactersCount).find { fourCharacters =>
    startOfMessageMarker += 1
    fourCharacters.distinct.length == distinctCharactersCount
  }
  println(startOfMessageMarker)
}
