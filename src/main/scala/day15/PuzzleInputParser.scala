package io.github.avapl
package day15

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Seq[(SensorPosition, BeaconPosition)]](day = 15) {

  override protected def parse(string: String): Seq[(SensorPosition, BeaconPosition)] =
    for {
      line <- string.splitLines
      s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" = line
    } yield ((sensorX.toInt, sensorY.toInt), (beaconX.toInt, beaconY.toInt))
}
