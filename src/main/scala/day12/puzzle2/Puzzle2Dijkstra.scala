package io.github.avapl
package day12.puzzle2

import day12.{Column, Dijkstra, HeightMap, Row}

class Puzzle2Dijkstra(heightMap: HeightMap)
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
    heightMap(currentRow)(currentColumn) - heightMap(targetRow)(targetColumn) <= 1
}
