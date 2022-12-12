package io.github.avapl
package day12

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[HeightMap](day = 12) {
  override protected def parse(string: String): HeightMap =
    for {
      line <- string.splitLines.toIndexedSeq
    } yield for {
      height <- line
    } yield height
}
