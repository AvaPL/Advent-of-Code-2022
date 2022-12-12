package io.github.avapl
package day12.puzzle1

import day12._

object Puzzle1 extends App {
  val heightMap = PuzzleInputParser.parsedInput
  val (startRow, startColumn) = markerRowColumn(heightMap, startMarker)
  val (endRow, endColumn) = markerRowColumn(heightMap, endMarker)
  val heightMapWithoutMarkers = removeMarkers(heightMap)
  val dijkstra = new Puzzle1Dijkstra(heightMapWithoutMarkers)
  val costs = dijkstra.costs(startRow, startColumn)
  val result = costs(endRow)(endColumn)
  println(result)
}
