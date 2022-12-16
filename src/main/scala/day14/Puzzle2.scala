package io.github.avapl
package day14

object Puzzle2 extends App {
  val initialCaveSystem = PuzzleInputParser.parsedInput
  val initialSourceSandRow = initialCaveSystem.indexWhere(_.contains(sand))
  val initialSourceSandColumn = initialCaveSystem(initialSourceSandRow).indexWhere(_ == sand)
  val initialCaveWidth = initialCaveSystem.head.size

  val caveExpansionLeft = initialCaveSystem.size - initialSourceSandColumn
  val caveExpansionRight = initialCaveSystem.size - (initialCaveWidth - initialSourceSandColumn)
  val floor = Seq.fill(caveExpansionLeft + initialCaveWidth + caveExpansionRight)(rock)
  val leftExpansionSpace = Seq.fill(caveExpansionLeft)(emptySpace)
  val rightExpansionSpace = Seq.fill(caveExpansionRight)(emptySpace)
  val caveSystemWithFloor =
    initialCaveSystem.map(leftExpansionSpace ++ _ ++ rightExpansionSpace).appended(floor).map(_.toArray).toArray
  val sourceSandRow = caveSystemWithFloor.indexWhere(_.contains(sand))
  val sourceSandColumn = caveSystemWithFloor(sourceSandRow).indexWhere(_ == sand)

  var currentSandRow = sourceSandRow
  var currentSandColumn = sourceSandColumn
  var isSourceBlocked = false
  var result = 1

  while (!isSourceBlocked)
    if (isEmptySpace(currentSandRow + 1, currentSandColumn))
      moveSand(currentSandRow + 1, currentSandColumn)
    else if (isEmptySpace(currentSandRow + 1, currentSandColumn - 1))
      moveSand(currentSandRow + 1, currentSandColumn - 1)
    else if (isEmptySpace(currentSandRow + 1, currentSandColumn + 1))
      moveSand(currentSandRow + 1, currentSandColumn + 1)
    else if (currentSandRow == sourceSandRow && currentSandColumn == sourceSandColumn)
      isSourceBlocked = true
    else
      createNewSand()

  println(result)

  private def isEmptySpace(row: Int, column: Int) =
    isIndexValid(row, column) &&
      caveSystemWithFloor(row)(column) == emptySpace

  private def isIndexValid(row: Int, column: Int) =
    0 <= row && row < caveSystemWithFloor.length &&
      0 <= column && column < caveSystemWithFloor.head.length

  private def moveSand(targetRow: Int, targetColumn: Int): Unit = {
    caveSystemWithFloor(currentSandRow)(currentSandColumn) = emptySpace
    currentSandRow = targetRow
    currentSandColumn = targetColumn
    caveSystemWithFloor(currentSandRow)(currentSandColumn) = sand
  }

  private def createNewSand(): Unit = {
    currentSandRow = sourceSandRow
    currentSandColumn = sourceSandColumn
    caveSystemWithFloor(currentSandRow)(currentSandColumn) = sand
    result += 1
  }
}
