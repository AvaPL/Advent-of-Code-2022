package io.github.avapl
package day17

import util.InputParser

object PuzzleInputParser extends InputParser[JetStream](day = 17) {
  override protected def parse(string: String): JetStream =
    string.map {
      case '<' => Left
      case '>' => Right
    }
}
