package io.github.avapl
package day15

object Puzzle2 extends App {
  val sensorToNearestBeacon = PuzzleInputParser.parsedInput
  val minXY = 0
  val (distressBeaconX, distressBeaconY) = LazyList
    .from(minXY)
    .map(y => (y, calculateCoveragesAtY(sensorToNearestBeacon)(y)))
    .collect { case (y, Seq(coverageRange, _)) =>
      val x = coverageRange.last + 1
      (x, y)
    }
    .head
  val result = distressBeaconX * 4_000_000L + distressBeaconY
  println(result)
}
