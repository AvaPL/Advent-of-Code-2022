package io.github.avapl
package day8

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Trees](day = 8) {
  override protected def parse(string: String): Trees =
    for {
      line <- string.splitLines.toIndexedSeq
    } yield for {
      character <- line.splitBy("").toIndexedSeq
    } yield character.toInt
}
