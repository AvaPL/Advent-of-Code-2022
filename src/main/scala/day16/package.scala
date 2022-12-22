package io.github.avapl

import scala.collection.mutable

package object day16 {

  type ValveName = String
  type ShortestPaths = Map[ValveName, Map[ValveName, Int]]

  case class Valve(name: ValveName, flowRate: Int, tunnels: Set[ValveName])

  def floydWarshall(valves: Seq[Valve]): ShortestPaths = {
    // https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm
    val distances = mutable.Map.empty[(ValveName, ValveName), Int].withDefaultValue(Int.MaxValue)
    for {
      u <- valves
      v <- u.tunnels
    } distances((u.name, v)) = 1
    for {
      k <- valves
      i <- valves
      j <- valves
      if distances((i.name, j.name)) > distances((i.name, k.name)).toLong + distances((k.name, j.name))
    } distances((i.name, j.name)) = distances((i.name, k.name)) + distances((k.name, j.name))

    distances.groupMapReduce { case ((from, _), _) => from } { case ((_, to), distance) => Map(to -> distance) }(_ ++ _)
  }

  def simulate(valves: Seq[Valve], shortestPaths: ShortestPaths, simulationMinutes: Int): Int = {
    var maxPressureReleased = 0

    def loop(
        currentValveName: ValveName,
        closedValvesByFlowRateDesc: Seq[Valve], // sorting improves performance
        remainingMinutes: Int,
        currentFlowRate: Int,
        pressureReleased: Int
    ): Unit =
      if (remainingMinutes == 0 || closedValvesByFlowRateDesc.isEmpty)
        maxPressureReleased = maxPressureReleased.max(pressureReleased + remainingMinutes * currentFlowRate)
      else {
        val upperBound =
          calculateUpperBound(closedValvesByFlowRateDesc, remainingMinutes, currentFlowRate, pressureReleased)
        if (upperBound > maxPressureReleased) {
          val (currentClosedValve, otherClosedValves) = closedValvesByFlowRateDesc.partition(_.name == currentValveName)
          currentClosedValve.foreach { currentValveClosed => // open current valve
            loop(
              currentValveName,
              otherClosedValves,
              remainingMinutes - 1,
              currentFlowRate + currentValveClosed.flowRate,
              pressureReleased + currentFlowRate
            )
          }
          otherClosedValves.foreach { case Valve(closedValveName, _, _) =>
            val pathMinutes = shortestPaths(currentValveName)(closedValveName)
            if (pathMinutes < remainingMinutes) // move to another valve
              loop(
                closedValveName,
                closedValvesByFlowRateDesc,
                remainingMinutes - pathMinutes,
                currentFlowRate,
                pressureReleased + currentFlowRate * pathMinutes
              )
            else // stay at current valve
              maxPressureReleased = maxPressureReleased.max(pressureReleased + remainingMinutes * currentFlowRate)
          }
        }
      }

    val closedValvesByFlowDesc = valves
      .filter(_.flowRate > 0) // there is no point in opening a valve with rate 0
      .sortBy(_.flowRate)(Ordering.Int.reverse)
    loop(
      currentValveName = "AA",
      closedValvesByFlowDesc,
      remainingMinutes = simulationMinutes,
      currentFlowRate = 0,
      pressureReleased = 0
    )

    maxPressureReleased
  }

  private def calculateUpperBound(
      closedValvesByFlowRateDesc: Seq[Valve],
      remainingMinutes: Int,
      currentFlowRate: Int,
      pressureReleased: Int
  ) = {
    val pressureWithCurrentFlowRate = pressureReleased + remainingMinutes * currentFlowRate
    val maxAdditionalPressureReleased = closedValvesByFlowRateDesc
      .map(_.flowRate)
      // open current valve (1 minute), move to next valve and open it (minimum 2 minutes), repeat
      .zip(remainingMinutes - 1 to 0 by -2)
      .map { case (flowRate, minutes) =>
        flowRate * minutes
      }
      .sum
    pressureWithCurrentFlowRate + maxAdditionalPressureReleased
  }
}
