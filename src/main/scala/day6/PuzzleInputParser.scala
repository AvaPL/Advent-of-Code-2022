package io.github.avapl
package day6

import util.InputParser

object PuzzleInputParser extends InputParser[Signal](day = 6) {
  override protected def parse(string: String): Signal = string
}
