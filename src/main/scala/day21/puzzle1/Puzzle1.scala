package io.github.avapl
package day21.puzzle1

import day21.{evaluateMonkeyValue, rootMonkeyName}

object Puzzle1 extends App {
  val monkeys = PuzzleInputParser.parsedInput
  val result = evaluateMonkeyValue(monkeys, rootMonkeyName)
  println(result)
}
