package io.github.avapl
package day17

object Puzzle1 extends App {
  val jetStream = PuzzleInputParser.parsedInput
  val jetIterator = Iterator.continually(jetStream).flatten
  val shapesIterator = Iterator.continually(shapes).flatten
  var chamber = Vector.empty[String]
  for (_ <- 0 until 2022)
    dropShape(shapesIterator.next())
  val result = calculateTowerHeight()
  println(result)

  private def dropShape(shape: Shape): Unit = {
    padChamber()
    var currentShape = shape
    var shapeIndex = 0
    var isResting = false
    while (!isResting) {
      currentShape = moveWithJet(currentShape, shapeIndex)
      isResting = collidesWithChamber(currentShape, shapeIndex + 1)
      if (!isResting)
        shapeIndex += 1
    }
    addShapeToChamber(currentShape, shapeIndex)
  }

  private def padChamber(): Unit = {
    val topRockIndex = chamber.indexWhere(_.contains('#'))
    val paddingLines = if (topRockIndex > 0) chamberTopHeight - topRockIndex else chamberTopHeight
    chamber = Vector.fill(paddingLines)(".......") ++ chamber
  }

  private def moveWithJet(currentShape: Shape, shapeIndex: Int) =
    jetIterator.next() match {
      case Left  => moveLeft(currentShape, shapeIndex)
      case Right => moveRight(currentShape, shapeIndex)
    }

  private def moveLeft(shape: Shape, shapeIndex: Int) =
    if (isOnLeftEdge(shape)) shape
    else {
      val movedLeft = shape.map(_.tail).map(_ + '.')
      if (collidesWithChamber(movedLeft, shapeIndex)) shape
      else movedLeft
    }

  private def isOnLeftEdge(shape: Shape) =
    shape.map(_.head).contains('#')

  private def collidesWithChamber(shape: Shape, index: Int) = {
    val collidesWithFloor = index + shapeHeight > chamber.length
    lazy val collidesWithTower = chamber
      .slice(index, index + shapeHeight)
      .zip(shape)
      .flatMap { case (chamberLine, shapeLine) =>
        chamberLine.zip(shapeLine)
      }
      .contains(('#', '#'))
    collidesWithFloor || collidesWithTower
  }

  private def moveRight(shape: Shape, shapeIndex: Int) =
    if (isOnRightEdge(shape)) shape
    else {
      val movedRight = shape.map(_.init).map('.' + _)
      if (collidesWithChamber(movedRight, shapeIndex)) shape
      else movedRight
    }

  private def isOnRightEdge(shape: Shape) =
    shape.map(_.last).contains('#')

  private def addShapeToChamber(shape: Shape, shapeIndex: Int): Unit =
    for (i <- shape.indices) {
      val chamberLine = chamber(shapeIndex + i)
      val shapeLine = shape(i)
      val updatedLine = chamberLine
        .zip(shapeLine)
        .map {
          case ('.', '.') => '.'
          case _          => '#'
        }
        .mkString
      chamber = chamber.updated(shapeIndex + i, updatedLine)
    }

  private def calculateTowerHeight() =
    chamber.dropWhile(!_.contains('#')).length
}
