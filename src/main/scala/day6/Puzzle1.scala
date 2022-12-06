package io.github.avapl
package day6

object Puzzle1 extends App {
  val signal = PuzzleInputParser.parsedInput
  val distinctCharactersCount = 4
  var startOfPacketMarker = distinctCharactersCount - 1
  signal.sliding(distinctCharactersCount).find { fourCharacters =>
    startOfPacketMarker += 1
    fourCharacters.distinct.length == distinctCharactersCount
  }
  println(startOfPacketMarker)
}
