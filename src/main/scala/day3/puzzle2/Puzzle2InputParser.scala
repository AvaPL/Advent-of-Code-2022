package io.github.avapl
package day3.puzzle2

import util.InputParser
import util.InputParser._

object Puzzle2InputParser extends InputParser[Seq[Rucksack]](day = 3) {
  override protected def parse(string: String): Seq[Rucksack] =
    for {
      line <- string.splitLines
    } yield line.toSet
}
