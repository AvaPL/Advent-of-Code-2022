package io.github.avapl
package day18

object Puzzle2 extends App {
  val cubePositions = PuzzleInputParser.parsedInput
  val result = calculateVisibleSides(dropletSurroundingCubes) - surroundingCubeVisibleSides
  println(result)

  private lazy val dropletSurroundingCubes = surroundingCube ++ Set
    .unfold(surroundingCube) { surroundingCubes =>
      val expansion = surroundingCubes.flatMap(expand).diff(surroundingCubes)
      Option.when(expansion.nonEmpty)((expansion, surroundingCubes ++ expansion))
    }
    .flatten

  private lazy val surroundingCube = {
    Set(
      for {
        y <- minY - 1 to maxY + 1
        z <- minZ - 1 to maxZ + 1
      } yield CubePosition(minX - 1, y, z),
      for {
        y <- minY - 1 to maxY + 1
        z <- minZ - 1 to maxZ + 1
      } yield CubePosition(maxX + 1, y, z),
      for {
        x <- minX - 1 to maxX + 1
        z <- minZ - 1 to maxZ + 1
      } yield CubePosition(x, minY - 1, z),
      for {
        x <- minX - 1 to maxX + 1
        z <- minZ - 1 to maxZ + 1
      } yield CubePosition(x, maxY + 1, z),
      for {
        x <- minX - 1 to maxX + 1
        y <- minY - 1 to maxY + 1
      } yield CubePosition(x, y, minZ - 1),
      for {
        x <- minX - 1 to maxX + 1
        y <- minY - 1 to maxY + 1
      } yield CubePosition(x, y, maxZ + 1)
    ).flatten
  }

  private lazy val minX = cubePositions.map(_.x).min
  private lazy val maxX = cubePositions.map(_.x).max
  private lazy val minY = cubePositions.map(_.y).min
  private lazy val maxY = cubePositions.map(_.y).max
  private lazy val minZ = cubePositions.map(_.z).min
  private lazy val maxZ = cubePositions.map(_.z).max

  private def expand(cubePosition: CubePosition) =
    adjacentCubePositions(cubePosition)
      .filter(isInsideSurroundingCube)
      .diff(cubePositions)

  private def isInsideSurroundingCube(cubePosition: CubePosition) =
    minX <= cubePosition.x && cubePosition.x <= maxX &&
      minY <= cubePosition.y && cubePosition.y <= maxY &&
      minZ <= cubePosition.z && cubePosition.z <= maxZ

  private lazy val surroundingCubeVisibleSides = 2 * (
    ((maxX - minX + 3) * (maxY - minY + 3)) +
      ((maxY - minY + 3) * (maxZ - minZ + 3)) +
      ((maxX - minX + 3) * (maxZ - minZ + 3))
  )
}
