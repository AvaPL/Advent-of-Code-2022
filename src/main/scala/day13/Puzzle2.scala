package io.github.avapl
package day13

import play.api.libs.json.Json

object Puzzle2 extends App {
  val packetPairs = PuzzleInputParser.parsedInput
  val dividerPacket1 = Json.parse("[[2]]")
  val dividerPacket2 = Json.parse("[[6]]")
  val packets = packetPairs.flatMap { case (packet1, packet2) =>
    Seq(packet1, packet2)
  } :+ dividerPacket1 :+ dividerPacket2
  val sortedPackets = packets.sortWith(comparePacketPair(_, _) < 0)
  val dividerPacket1Index = sortedPackets.indexOf(dividerPacket1) + 1
  val dividerPacket2Index = sortedPackets.indexOf(dividerPacket2) + 1
  val result = dividerPacket1Index * dividerPacket2Index
  println(result)
}
