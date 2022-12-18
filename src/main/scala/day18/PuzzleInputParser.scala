package io.github.avapl
package day18

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Set[CubePosition]](day = 18) {
  override protected def parse(string: String): Set[CubePosition] = {
    for {
      line <- string.splitLines
      s"$x,$y,$z" = line
    } yield CubePosition(x.toInt, y.toInt, z.toInt)
  }.toSet
}
