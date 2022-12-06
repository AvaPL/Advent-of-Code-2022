package io.github.avapl
package day6

object Puzzle1 extends App {
  val signal = PuzzleInputParser.parsedInput
  val distinctCharactersCount = 4
  var startOfPacketMarker = distinctCharactersCount - 1
  signal.sliding(distinctCharactersCount).find { characters =>
    startOfPacketMarker += 1
    characters.distinct.length == distinctCharactersCount
  }
  println(startOfPacketMarker)
}
