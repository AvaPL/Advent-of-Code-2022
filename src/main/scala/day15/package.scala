package io.github.avapl

import scala.math.abs

package object day15 {
  type Position = (Int, Int)
  type SensorPosition = Position
  type BeaconPosition = Position

  def calculateCoveragesAtY(sensorToNearestBeacon: Seq[(SensorPosition, BeaconPosition)])(y: Int): Seq[Range] =
    reduceRanges {
      sensorToNearestBeacon.map { case (sensorPosition, nearestBeaconPosition) =>
        coverageAtY(sensorPosition, nearestBeaconPosition)(y)
      }
    }

  private def reduceRanges(ranges: Seq[Range]) =
    ranges.filter(_.nonEmpty).sortBy(_.head).foldLeft(Seq.empty[Range]) {
      case (init :+ last, range) if last.head.max(range.head) <= last.last.min(range.last) =>
        init :+ (last.head.min(range.head) to last.last.max(range.last))
      case (reducedRanges, range) => reducedRanges :+ range
    }

  private def coverageAtY(sensorPosition: SensorPosition, nearestBeaconPosition: BeaconPosition)(y: Int) = {
    val (sensorX, sensorY) = sensorPosition
    val (beaconX, beaconY) = nearestBeaconPosition
    val manhattanDistance = abs(sensorX - beaconX) + abs(sensorY - beaconY)
    val distanceToY = abs(sensorY - y)
    val maxX = manhattanDistance - distanceToY
    (sensorX - maxX) to (sensorX + maxX)
  }
}
