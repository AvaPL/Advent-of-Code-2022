package io.github.avapl

import scala.util.chaining._

package object day23 {

  sealed abstract class Direction(val rowDirection: Int, val columnDirection: Int)
  case object N extends Direction(-1, 0)
  case object S extends Direction(1, 0)
  case object W extends Direction(0, -1)
  case object E extends Direction(0, 1)
  case object NW extends Direction(-1, -1)
  case object NE extends Direction(-1, 1)
  case object SW extends Direction(1, -1)
  case object SE extends Direction(1, 1)

  case class Position(row: Int, column: Int) {
    def +(direction: Direction): Position =
      Position(row + direction.rowDirection, column + direction.columnDirection)
  }

  private val moveDirections = IndexedSeq(N, S, W, E)

  def executeRound(elfPositions: Set[Position], round: Int): Set[Position] = {
    val proposedPositions = elfPositions.toSeq.flatMap { from =>
      for {
        to <- proposeMoveIfHasNeighbour(from, round, elfPositions)
      } yield from -> to
    }
    val (fromPositions, toPositions) = filterByUniqueTargetPosition(proposedPositions).pipe { fromToMap =>
      (fromToMap.keySet, fromToMap.values.toSet)
    }
    elfPositions -- fromPositions ++ toPositions
  }

  private def filterByUniqueTargetPosition(proposedPositions: Seq[(Position, Position)]) =
    proposedPositions
      .groupBy { case (_, to) =>
        to
      }
      .collect { case (_, Seq(fromTo)) =>
        fromTo
      }

  private def proposeMoveIfHasNeighbour(position: Position, round: Int, elfPositions: Set[Position]) = {
    val adjacentPositions = Set(N, S, W, E, NW, NE, SW, SE).map(position + _)
    val hasNeighbour = adjacentPositions.intersect(elfPositions).nonEmpty
    if (hasNeighbour)
      proposeMove(position, round, elfPositions)
    else None
  }

  private def proposeMove(position: Position, round: Int, elfPositions: Set[Position]) =
    moveDirections.indices
      .map { i =>
        val moveDirection = moveDirections((i + round) % moveDirections.size)
        val collidingDirections: Set[Direction] = moveDirection match {
          case N => Set(N, NE, NW)
          case S => Set(S, SE, SW)
          case W => Set(W, NW, SW)
          case E => Set(E, NE, SE)
        }
        Option.when(areAdjacentPositionsEmpty(position, collidingDirections, elfPositions))(moveDirection)
      }
      .collectFirst { case Some(direction) =>
        position + direction
      }

  private def areAdjacentPositionsEmpty(position: Position, directions: Set[Direction], elfPositions: Set[Position]) = {
    val positionsToCheck = directions.map(position + _)
    positionsToCheck.intersect(elfPositions).isEmpty
  }
}
