package io.github.avapl
package day4

object Puzzle1 extends App {
  val sectionAssignments = PuzzleInputParser.parsedInput
  val result = sectionAssignments.count { case (sectionAssignment1, sectionAssignment2) =>
    sectionAssignment1.containsSlice(sectionAssignment2) || sectionAssignment2.containsSlice(sectionAssignment1)
  }
  println(result)
}
