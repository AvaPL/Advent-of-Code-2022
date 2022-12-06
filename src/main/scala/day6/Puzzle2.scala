package io.github.avapl
package day6

object Puzzle2 extends App {
  val signal = PuzzleInputParser.parsedInput
  val distinctCharactersCount = 14
  var startOfMessageMarker = distinctCharactersCount - 1
  signal.sliding(distinctCharactersCount).find { characters =>
    startOfMessageMarker += 1
    characters.distinct.length == distinctCharactersCount
  }
  println(startOfMessageMarker)
}
