package io.github.avapl
package day16

object Puzzle1 extends App {
  val valves = PuzzleInputParser.parsedInput
  val shortestPaths = floydWarshall(valves)
  val valvesWithPositiveFlowRate = valves.filter(_.flowRate > 0)
  val result = simulate(valvesWithPositiveFlowRate, shortestPaths, simulationMinutes = 30)
  println(result)
}
