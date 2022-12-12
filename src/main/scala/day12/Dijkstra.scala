package io.github.avapl
package day12

import scala.collection.mutable

abstract class Dijkstra(rowsCount: Int, columnsCount: Int) {

  protected def isStepValid(currentRow: Row, currentColumn: Column, targetRow: Row, targetColumn: Column): Boolean

  def costs(startRow: Row, startColumn: Column): Costs = {
    val costs = Array.fill(rowsCount)(Array.fill(columnsCount)(Int.MaxValue))
    val visited = mutable.PriorityQueue[((Row, Column), Cost)]()(Ordering.by { case (_, cost) => cost })
    visited.enqueue(((startRow, startColumn), 0))
    while (visited.nonEmpty) {
      val ((currentRow, currentColumn), currentCost) = visited.dequeue
      getPossibleSteps(currentRow, currentColumn).collect { case (targetRow, targetColumn) =>
        val newCost = currentCost + 1
        if (newCost < costs(targetRow)(targetColumn)) {
          costs(targetRow)(targetColumn) = newCost
          visited.enqueue(((targetRow, targetColumn), newCost))
        }
      }
    }
    costs.map(_.toIndexedSeq).toIndexedSeq
  }

  private def getPossibleSteps(currentRow: Row, currentColumn: Column) =
    Seq(
      (currentRow, currentColumn + 1),
      (currentRow, currentColumn - 1),
      (currentRow - 1, currentColumn),
      (currentRow + 1, currentColumn)
    ).filter { case (targetRow, targetColumn) =>
      isIndexValid(targetRow, targetColumn) && isStepValid(currentRow, currentColumn, targetRow, targetColumn)
    }

  private def isIndexValid(row: Row, column: Column) =
    0 <= row && row < rowsCount &&
      0 <= column && column < columnsCount
}
