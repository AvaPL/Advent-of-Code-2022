package io.github.avapl
package day19

object Puzzle1 extends App {
  val blueprints = PuzzleInputParser.parsedInput
  val result = blueprints.map { blueprint =>
    val maxGeodes = simulate(blueprint, totalMinutes = 24)
    blueprint.id * maxGeodes
  }.sum
  println(result)
}
