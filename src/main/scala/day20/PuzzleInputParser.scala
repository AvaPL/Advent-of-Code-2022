package io.github.avapl
package day20

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[IndexedSeq[Long]](day = 20) {
  override protected def parse(string: String): IndexedSeq[Long] =
    string.splitLines.map(_.toLong).toIndexedSeq
}
