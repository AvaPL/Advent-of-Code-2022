package io.github.avapl
package day24

import scala.collection.mutable
import scala.math.floorMod

class Valley(width: Int, height: Int, blizzards: Map[Position, Seq[Blizzard]]) {

  val entrancePosition: Position = Position(-1, 0)
  val exitPosition: Position = Position(height, width - 1)

  def findShortestPath(startingPosition: Position, targetPosition: Position, startingMinute: Int): Int = {
    var shortestPath = Int.MaxValue
    val positionPathQueue = mutable.Queue((startingPosition, startingMinute))

    while (positionPathQueue.nonEmpty) {
      val (_, batchPathLength) = positionPathQueue.head
      val positionsBatch = positionPathQueue
        .dequeueWhile { case (_, pathLength) => pathLength == batchPathLength }
        .map { case (position, _) => position }
        .toSet
      if (positionsBatch.contains(targetPosition))
        shortestPath = batchPathLength
      else
        nextValidPositions(positionsBatch, batchPathLength).foreach { position =>
          positionPathQueue.enqueue((position, batchPathLength + 1))
        }
    }

    shortestPath
  }

  private def nextValidPositions(positionsBatch: Set[Position], batchPathLength: Int) = {
    val nextBlizzardPositions = blizzardPositions(batchPathLength + 1)
    positionsBatch.flatMap { position =>
      (adjacentPositions(position) + position).filter(isPositionValid).diff(nextBlizzardPositions)
    }
  }

  private def blizzardPositions(pathLength: Int) = {
    for {
      (Position(row, column), blizzards) <- blizzards
      Blizzard(rowDirection, columnDirection) <- blizzards
    } yield {
      val newRow = floorMod(row + rowDirection * pathLength, height)
      val newColumn = floorMod(column + columnDirection * pathLength, width)
      Position(newRow, newColumn)
    }
  }.toSet

  private def adjacentPositions(position: Position) =
    Set(
      position.copy(row = position.row - 1),
      position.copy(row = position.row + 1),
      position.copy(column = position.column - 1),
      position.copy(column = position.column + 1)
    )

  private def isPositionValid(position: Position) = {
    val Position(row, column) = position
    0 <= row && row < height && 0 <= column && column < width ||
    position == entrancePosition ||
    position == exitPosition
  }
}
