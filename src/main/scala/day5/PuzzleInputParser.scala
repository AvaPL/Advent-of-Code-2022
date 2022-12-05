package io.github.avapl
package day5

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[(IndexedSeq[Stack[Crate]], Seq[RearrangementStep])](day = 5) {

  override protected def parse(string: String): (IndexedSeq[Stack[Crate]], Seq[RearrangementStep]) = {
    val Seq(stacksBlock, rearrangementStepsBlock) = string.splitBlocks
    val stacks = parseStacks(stacksBlock)
    val rearrangementSteps = parseRearrangementSteps(rearrangementStepsBlock)
    (stacks.toIndexedSeq, rearrangementSteps)
  }

  private def parseStacks(stacksBlock: String) = {
    val lines = stacksBlock.splitLines.dropRight(1)
    val maxLineLength = lines.map(_.length).max
    val paddedLines = lines.map(_.padTo(maxLineLength, ' '))
    for {
      stackLine <- paddedLines.transpose if stackLine.exists(_.isLetter)
    } yield stackLine.filter(_.isLetter).toList
  }

  private val rearrangementStepRegex = """move (\d+) from (\d+) to (\d+)""".r

  private def parseRearrangementSteps(rearrangementProcedureBlock: String) =
    for {
      rearrangementStepString <- rearrangementProcedureBlock.splitLines
      rearrangementStepRegex(quantity, sourceStackNumber, targetStackNumber) = rearrangementStepString
    } yield RearrangementStep(quantity.toInt, sourceStackNumber.toInt, targetStackNumber.toInt)
}
