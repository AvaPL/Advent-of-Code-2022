package io.github.avapl
package day16

import scala.collection.mutable

object Puzzle1 extends App {
  val valves = PuzzleInputParser.parsedInput
  val shortestPaths = floydWarshall(valves)
  val result = simulate(valves)
  println(result)

  private def floydWarshall(valves: Seq[Valve]): Map[ValveName, Map[ValveName, Int]] = {
    // https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm
    val distances = mutable.Map.empty[(ValveName, ValveName), Int].withDefaultValue(Int.MaxValue)
    for {
      u <- valves
      v <- u.tunnels
    } distances((u.name, v)) = 1
    for (v <- valves)
      distances((v.name, v.name)) = 0
    for {
      k <- valves
      i <- valves
      j <- valves
      if distances((i.name, j.name)) > distances((i.name, k.name)).toLong + distances((k.name, j.name))
    } distances((i.name, j.name)) = distances((i.name, k.name)) + distances((k.name, j.name))

    distances.groupMapReduce { case ((from, _), _) => from } {
      // remove path to itself
      case ((from, to), distance) if from != to => Map(to -> distance)
      case _                                    => Map.empty[ValveName, Int]
    }(_ ++ _)
  }

  private def simulate(valves: Seq[Valve]): Int = {
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
      remainingMinutes = 30,
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
