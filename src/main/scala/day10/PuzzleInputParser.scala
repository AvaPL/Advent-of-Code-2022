package io.github.avapl
package day10

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Seq[Instruction]](day = 10) {
  override protected def parse(string: String): Seq[Instruction] =
    for {
      line <- string.splitLines
    } yield line match {
      case "noop"         => Noop
      case s"addx $value" => AddX(value.toInt)
    }
}
