package io.github.avapl
package day9

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Seq[Motion]](day = 9) {

  override protected def parse(string: String): Seq[Motion] =
    for {
      line <- string.splitLines
      Seq(direction, count) = line.splitBy(" ")
      motion <- parseMotions(direction, count)
    } yield motion

  private def parseMotions(directionString: String, countString: String) = {
    val count = countString.toInt
    val motion = directionString match {
      case "R" => Right
      case "L" => Left
      case "U" => Up
      case "D" => Down
    }
    Seq.fill(count)(motion)
  }
}
