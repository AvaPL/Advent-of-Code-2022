package io.github.avapl
package day14

object Puzzle1 extends App {
  val caveSystem = PuzzleInputParser.parsedInput
  var sourceSandRow = caveSystem.indexWhere(_.contains(sand))
  var sourceSandColumn = caveSystem(sourceSandRow).indexWhere(_ == sand)

  val mutableCaveSystem = caveSystem.map(_.toArray).toArray

  var currentSandRow = sourceSandRow
  var currentSandColumn = sourceSandColumn
  var result = 0

  while (!isOutOfBounds)
    if (isEmptySpace(currentSandRow + 1, currentSandColumn))
      moveSand(currentSandRow + 1, currentSandColumn)
    else if (isEmptySpace(currentSandRow + 1, currentSandColumn - 1))
      moveSand(currentSandRow + 1, currentSandColumn - 1)
    else if (isEmptySpace(currentSandRow + 1, currentSandColumn + 1))
      moveSand(currentSandRow + 1, currentSandColumn + 1)
    else
      createNewSand()

  println(result)

  private def isOutOfBounds =
    currentSandColumn == 0 ||
      currentSandColumn == caveSystem.head.size - 1 ||
      currentSandRow == caveSystem.size - 1

  private def isEmptySpace(row: Int, column: Int) =
    isIndexValid(row, column) &&
      mutableCaveSystem(row)(column) == emptySpace

  private def isIndexValid(row: Int, column: Int) =
    0 <= row && row < caveSystem.size &&
      0 <= column && column < caveSystem.head.size

  private def moveSand(targetRow: Int, targetColumn: Int): Unit = {
    mutableCaveSystem(currentSandRow)(currentSandColumn) = emptySpace
    currentSandRow = targetRow
    currentSandColumn = targetColumn
    mutableCaveSystem(currentSandRow)(currentSandColumn) = sand
  }

  private def createNewSand(): Unit = {
    currentSandRow = sourceSandRow
    currentSandColumn = sourceSandColumn
    mutableCaveSystem(currentSandRow)(currentSandColumn) = sand
    result += 1
  }
}
