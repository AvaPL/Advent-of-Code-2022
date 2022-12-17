package io.github.avapl
package day15

object Puzzle2 extends App {
  val sensorToNearestBeacon = PuzzleInputParser.parsedInput
  val minXY = 0
  val (distressBeaconX, distressBeaconY) = LazyList
    .from(minXY)
    .map(y => (y, calculateCoveragesAtY(sensorToNearestBeacon)(y)))
    .collectFirst { case (y, Seq(coverageRange, _)) =>
      val x = coverageRange.last + 1
      (x, y)
    }
    .get
  val result = distressBeaconX * 4_000_000L + distressBeaconY
  println(result)
}
