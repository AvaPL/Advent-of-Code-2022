package io.github.avapl
package day23

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Set[Position]](day = 23) {

  override protected def parse(string: String): Set[Position] = {
    val lines = string.splitLines
    val height = lines.length
    val width = lines.head.length
    for {
      row <- 0 until height
      column <- 0 until width
      if lines(row)(column) == '#'
    } yield Position(row, column)
  }.toSet
}
