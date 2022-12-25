package io.github.avapl
package day25

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Seq[String]](day = 25) {
  override protected def parse(string: String): Seq[String] = string.splitLines
}
