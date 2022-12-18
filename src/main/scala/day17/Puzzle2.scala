package io.github.avapl
package day17

import scala.collection.mutable

object Puzzle2 extends App {
  val jetStream = PuzzleInputParser.parsedInput
  val jetIterator = Iterator.continually(jetStream.zipWithIndex).flatten
  val shapesIterator = Iterator.continually(shapes.zipWithIndex).flatten

  val totalShapes = 1_000_000_000_000L

  var chamber = Vector.empty[String]
  type ChamberTop = Vector[String]
  type JetIteratorIndex = Int
  type ShapesIteratorIndex = Int
  type ChamberState = (ChamberTop, JetIteratorIndex, ShapesIteratorIndex)
  type Height = Int
  type ShapesCount = Int
  val chamberStates = mutable.Map.empty[ChamberState, (Height, ShapesCount)]

  var numberOfShapesUntilDuplication = -1
  var duplicatedSegmentHeight = 0
  var duplicatedSegmentShapesCount = 0
  var droppedShapes = 0

  while (numberOfShapesUntilDuplication < 0) {
    val (shape, shapesIteratorIndex) = shapesIterator.next()
    val (chamberRowIndex, jetIteratorIndex) = dropShape(shape)
    memoizeState(chamberRowIndex, jetIteratorIndex, shapesIteratorIndex)
  }
  val numberOfDuplicatedSegments = (totalShapes - numberOfShapesUntilDuplication) / duplicatedSegmentShapesCount + 1
  val remainingShapesToDrop = (totalShapes - numberOfShapesUntilDuplication) % duplicatedSegmentShapesCount - 1
  for (_ <- 0L until remainingShapesToDrop) {
    val (shape, _) = shapesIterator.next()
    dropShape(shape)
  }
  val towerHeightWithoutDuplicates =
    if (remainingShapesToDrop > 0)
      chamber.dropWhile(!_.contains('#')).length // Tower with all shapes
    else
      chamber.drop(chamberTopHeight).length // Tower without an extra top shape
  val result = towerHeightWithoutDuplicates + (numberOfDuplicatedSegments - 1) * duplicatedSegmentHeight
  println(result)

  private def dropShape(shape: Shape) = {
    padChamber()
    var currentShape = shape
    var currentJetIteratorIndex = -1
    var chamberRowIndex = 0
    var isResting = false
    while (!isResting) {
      val (movedShape, jetIteratorIndex) = moveWithJet(currentShape, chamberRowIndex)
      currentShape = movedShape
      currentJetIteratorIndex = jetIteratorIndex
      isResting = collidesWithChamber(currentShape, chamberRowIndex + 1)
      if (!isResting)
        chamberRowIndex += 1
    }
    addShapeToChamber(currentShape, chamberRowIndex)
    droppedShapes += 1
    (chamberRowIndex, currentJetIteratorIndex)
  }

  private def padChamber(): Unit = {
    val topRockIndex = chamber.indexWhere(_.contains('#'))
    val paddingLines = if (topRockIndex > 0) chamberTopHeight - topRockIndex else chamberTopHeight
    chamber = Vector.fill(paddingLines)(".......") ++ chamber
  }

  private def moveWithJet(currentShape: Shape, shapeIndex: Int) = {
    val (jetDirection, jetIteratorIndex) = jetIterator.next()
    val movedShape = jetDirection match {
      case Left  => moveLeft(currentShape, shapeIndex)
      case Right => moveRight(currentShape, shapeIndex)
    }
    (movedShape, jetIteratorIndex)
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

  private def moveRight(shape: Shape, chamberRowIndex: Int) =
    if (isOnRightEdge(shape)) shape
    else {
      val movedRight = shape.map(_.init).map('.' + _)
      if (collidesWithChamber(movedRight, chamberRowIndex)) shape
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

  private def memoizeState(chamberRowIndex: Int, jetIteratorIndex: Int, shapesIteratorIndex: Int): Unit = {
    val chamberTop = chamber.take(chamberTopHeight)
    val state = (chamberTop, jetIteratorIndex, shapesIteratorIndex)
    val height = chamber.length - chamberRowIndex
    if (chamberStates.contains(state)) {
      numberOfShapesUntilDuplication = droppedShapes - 1
      val (stateHeight, stateShapesCount) = chamberStates(state)
      duplicatedSegmentHeight = height - stateHeight
      duplicatedSegmentShapesCount = droppedShapes - stateShapesCount
    } else if (droppedShapes > 1) // The first shape collides with the ground
      chamberStates(state) = (height, droppedShapes)
  }
}
