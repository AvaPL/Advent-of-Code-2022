package io.github.avapl
package day6

object Puzzle1 extends App {
  val signal = PuzzleInputParser.parsedInput
  var startOfPacketMarker = 3
  signal.sliding(4).find { fourCharacters =>
    startOfPacketMarker += 1
    fourCharacters.distinct.length == 4
  }
  println(startOfPacketMarker)
}
