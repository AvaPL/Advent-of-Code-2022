package io.github.avapl

import scala.math.{abs, signum}

package object day9 {
  type Position = (Int, Int)
  type Rope = Seq[Position]

  sealed trait Motion
  case object Left extends Motion
  case object Right extends Motion
  case object Up extends Motion
  case object Down extends Motion

  def countUniqueTailPositions(rope: Rope, motions: Seq[Motion]): Int = {
    val positions = motions.scanLeft(rope) { case (rope, motion) =>
      moveRope(rope, motion)
    }
    positions.map(_.last).distinct.size
  }

  private def moveRope(rope: Rope, motion: Motion) = {
    val nextHeadPosition = moveHead(rope.head, motion)
    rope.tail.foldLeft(Seq(nextHeadPosition)) { case (movedRope, segmentPosition) =>
      val nextSegmentPosition = moveTailSegment(segmentPosition, movedRope.last)
      movedRope :+ nextSegmentPosition
    }
  }

  private def moveHead(headPosition: Position, motion: Motion) = {
    val (x, y) = headPosition
    motion match {
      case Left  => (x - 1, y)
      case Right => (x + 1, y)
      case Up    => (x, y + 1)
      case Down  => (x, y - 1)
    }
  }

  private def moveTailSegment(segmentPosition: Position, nextParentSegmentPosition: Position) =
    if (isTouching(segmentPosition, nextParentSegmentPosition))
      segmentPosition
    else {
      val (x, y) = segmentPosition
      val (nextParentX, nextParentY) = nextParentSegmentPosition
      (x + signum(nextParentX - x), y + signum(nextParentY - y))
    }

  private def isTouching(first: Position, second: Position) = {
    val (firstX, firstY) = first
    val (secondX, secondY) = second
    abs(firstX - secondX) <= 1 && abs(firstY - secondY) <= 1
  }
}
