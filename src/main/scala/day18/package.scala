package io.github.avapl

import scala.util.chaining._

package object day18 {
  case class CubePosition(x: Int, y: Int, z: Int)

  def calculateVisibleSides(cubePositions: Set[CubePosition]): Int =
    cubePositions
      .foldLeft((Set.empty[CubePosition], 0)) { case ((cubePositions, visibleSides), cubePosition) =>
        val adjacentCubesCount = cubePositions.intersect(adjacentCubePositions(cubePosition)).size
        (cubePositions + cubePosition, visibleSides + 6 - 2 * adjacentCubesCount)
      }
      .pipe { case (_, visibleSides) =>
        visibleSides
      }

  def adjacentCubePositions(cubePosition: CubePosition) =
    Set(
      cubePosition.copy(x = cubePosition.x - 1),
      cubePosition.copy(x = cubePosition.x + 1),
      cubePosition.copy(y = cubePosition.y - 1),
      cubePosition.copy(y = cubePosition.y + 1),
      cubePosition.copy(z = cubePosition.z - 1),
      cubePosition.copy(z = cubePosition.z + 1)
    )
}
