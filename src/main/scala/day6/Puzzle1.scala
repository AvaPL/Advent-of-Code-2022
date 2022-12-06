package io.github.avapl
package day6

object Puzzle1 extends App {
  val signal = PuzzleInputParser.parsedInput
  val distinctCharactersCount = 4
  val result = signal.sliding(distinctCharactersCount).indexWhere { characters =>
    characters.distinct.length == distinctCharactersCount
  } + distinctCharactersCount
  println(result)
}
