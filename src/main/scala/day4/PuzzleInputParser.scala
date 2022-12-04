package io.github.avapl
package day4

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Seq[(SectionAssignment, SectionAssignment)]](day = 4) {

  override protected def parse(string: String): Seq[(SectionAssignment, SectionAssignment)] =
    for {
      line <- string.splitLines
    } yield parseSectionAssignmentPair(line)

  private def parseSectionAssignmentPair(sectionAssignmentPairString: String) =
    sectionAssignmentPairString.splitBy(",").map(parseSectionAssignment) match {
      case Seq(sectionAssignment1, sectionAssignment2) => (sectionAssignment1, sectionAssignment2)
    }

  private def parseSectionAssignment(sectionAssignmentString: String) =
    sectionAssignmentString.splitBy("-").map(_.toInt) match {
      case Seq(start, end) => start to end
    }
}
