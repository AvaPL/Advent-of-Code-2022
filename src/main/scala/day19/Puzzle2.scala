package io.github.avapl
package day19

object Puzzle2 extends App {
  val blueprints = PuzzleInputParser.parsedInput.take(3)
  val result = blueprints.map { blueprint =>
    simulate(blueprint, totalMinutes = 32)
  }.product
  println(result)
}
