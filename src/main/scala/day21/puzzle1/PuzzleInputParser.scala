package io.github.avapl
package day21.puzzle1

import day21.{Monkey, OperationMonkey, ValueMonkey}
import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Seq[Monkey]](day = 21) {

  override protected def parse(string: String): Seq[Monkey] =
    for {
      line <- string.splitLines
    } yield parseMonkey(line)

  private def parseMonkey(line: String) =
    line match {
      case s"$name: $leftMonkey $operator $rightMonkey" =>
        val operation: (Long, Long) => Long = operator match {
          case "+" => _ + _
          case "-" => _ - _
          case "*" => _ * _
          case "/" => _ / _
        }
        OperationMonkey(name, leftMonkey, rightMonkey, operation)
      case s"$name: $value" => ValueMonkey(name, value.toInt)
    }
}
