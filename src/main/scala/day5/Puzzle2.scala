package io.github.avapl
package day5

object Puzzle2 extends App {
  val (stacks, rearrangementSteps) = PuzzleInputParser.parsedInput
  val rearrangedStacks = rearrangementSteps.foldLeft(stacks) { case (stacks, rearrangementStep) =>
    rearrangeStacks(stacks, rearrangementStep)
  }
  val result = rearrangedStacks.map(_.head).mkString
  println(result)

  private def rearrangeStacks(stacks: IndexedSeq[Stack[Crate]], rearrangementStep: RearrangementStep) = {
    val RearrangementStep(quantity, sourceStackNumber, targetStackNumber) = rearrangementStep
    val sourceStackIndex = sourceStackNumber - 1
    val targetStackIndex = targetStackNumber - 1
    val sourceStack = stacks(sourceStackIndex)
    val targetStack = stacks(targetStackIndex)
    val (pickedCrates, updatedSourceStack) = sourceStack.splitAt(quantity)
    val updatedTargetStack = pickedCrates ++ targetStack
    stacks
      .updated(sourceStackIndex, updatedSourceStack)
      .updated(targetStackIndex, updatedTargetStack)
  }
}
