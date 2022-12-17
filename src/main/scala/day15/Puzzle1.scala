package io.github.avapl
package day15

object Puzzle1 extends App {
  val sensorToNearestBeacon = PuzzleInputParser.parsedInput
  val y = 2_000_000
  val coverages = calculateCoveragesAtY(sensorToNearestBeacon)(y)
  val coveragesLength = coverages.map(_.length).sum
  val result = coveragesLength - beaconsAtY
  println(result)

  private lazy val beaconsAtY =
    sensorToNearestBeacon
      .collect { case (_, beaconPosition) =>
        beaconPosition
      }
      .distinct
      .count { case (_, beaconY) =>
        beaconY == y
      }
}
