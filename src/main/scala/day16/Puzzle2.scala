package io.github.avapl
package day16

object Puzzle2 extends App {
  val valves = PuzzleInputParser.parsedInput
  val shortestPaths = floydWarshall(valves)
  val valvesWithPositiveFlowRate = valves.filter(_.flowRate > 0)
  val valvesCombinations = for {
    combinationSize <- LazyList.from(1).takeWhile(_ < valvesWithPositiveFlowRate.size)
    myValves <- valvesWithPositiveFlowRate.combinations(combinationSize)
    elephantValves = valvesWithPositiveFlowRate.diff(myValves)
  } yield (myValves, elephantValves)
  val result = valvesCombinations.map { case (myValves, elephantValves) =>
    val myPressure = simulate(myValves, shortestPaths, simulationMinutes = 26)
    val elephantPressure = simulate(elephantValves, shortestPaths, simulationMinutes = 26)
    myPressure + elephantPressure
  }.max
  println(result)
}
