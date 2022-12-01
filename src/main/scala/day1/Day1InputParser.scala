package io.github.avapl
package day1

import util.InputParser
import util.InputParser._

object Day1InputParser extends InputParser[Seq[ElfFood]](day = 1) {
  override def parse(string: String): Seq[ElfFood] =
    for {
      block <- string.splitBlocks
    } yield for {
      blockLine <- block.splitLines
    } yield blockLine.toInt
}
