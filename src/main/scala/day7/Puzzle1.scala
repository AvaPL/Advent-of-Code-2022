package io.github.avapl
package day7

object Puzzle1 extends App {
  val fileSystem = PuzzleInputParser.parsedInput
  val result = fileSystem.keySet.toSeq.map(directorySize(fileSystem)).filter(_ <= 100_000).sum
  println(result)
}
