package io.github.avapl
package day21.puzzle2

import day21.evaluateMonkeyValue

object Puzzle2 extends App {
  val monkeys = PuzzleInputParser.parsedInput
  val result = evaluateMonkeyValue(monkeys, myName)
  println(result)
}
