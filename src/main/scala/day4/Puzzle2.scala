package io.github.avapl
package day4

object Puzzle2 extends App {
  val sectionAssignments = PuzzleInputParser.parsedInput
  val result = sectionAssignments.count { case (sectionAssignment1, sectionAssignment2) =>
    sectionAssignment1.intersect(sectionAssignment2).nonEmpty
  }
  println(result)
}
