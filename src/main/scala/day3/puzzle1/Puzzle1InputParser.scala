package io.github.avapl
package day3.puzzle1

import util.InputParser
import util.InputParser._

object Puzzle1InputParser extends InputParser[Seq[Rucksack]](day = 3) {

  override protected def parse(string: String): Seq[(Compartment, Compartment)] =
    for {
      line <- string.splitLines
      (leftHalf, rightHalf) = splitInHalf(line)
    } yield (leftHalf.toSet, rightHalf.toSet)

  private def splitInHalf(string: String) = {
    val halfLength = string.length / 2
    (string.take(halfLength), string.takeRight(halfLength))
  }
}
