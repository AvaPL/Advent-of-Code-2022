package io.github.avapl
package day13

object Puzzle1 extends App {
  val packetPairs = PuzzleInputParser.parsedInput
  val packetPairComparisons = packetPairs.map { case (packet1, packet2) =>
    comparePacketPair(packet1, packet2)
  }
  val result = packetPairComparisons.zipWithIndex.collect {
    case (comparison, index) if comparison < 0 => index + 1
  }.sum
  println(result)
}
