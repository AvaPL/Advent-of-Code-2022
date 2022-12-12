package io.github.avapl
package day12.puzzle1

import day12.{Column, Dijkstra, HeightMap, Row}

class Puzzle1Dijkstra(heightMap: HeightMap)
    extends Dijkstra(
      rowsCount = heightMap.size,
      columnsCount = heightMap.head.size
    ) {

  override protected def isStepValid(
      currentRow: Row,
      currentColumn: Column,
      targetRow: Row,
      targetColumn: Column
  ): Boolean =
    heightMap(targetRow)(targetColumn) - heightMap(currentRow)(currentColumn) <= 1
}
