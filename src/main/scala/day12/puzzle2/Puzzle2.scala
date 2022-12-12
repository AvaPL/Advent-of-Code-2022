package io.github.avapl
package day12.puzzle2

import day12._

object Puzzle2 extends App {
  val heightMap = PuzzleInputParser.parsedInput
  val (endRow, endColumn) = markerRowColumn(heightMap, endMarker)
  val heightMapWithoutMarkers = removeMarkers(heightMap)
  val dijkstra = new DownhillDijkstra(heightMapWithoutMarkers)
  val costs = dijkstra.costs(endRow, endColumn)
  val result = lowestStartingPositionCost(costs)
  println(result)

  private def lowestStartingPositionCost(costs: Costs) = {
    for {
      row <- heightMap.indices
      column <- heightMap.head.indices if heightMapWithoutMarkers(row)(column) == 'a'
    } yield costs(row)(column)
  }.min
}
