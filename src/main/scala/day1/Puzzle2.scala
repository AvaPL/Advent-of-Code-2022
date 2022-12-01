package io.github.avapl
package day1

object Puzzle2 extends App {
  val elfFoods = Day1InputParser.parsedInput
  val result = elfFoods.map(_.sum).sorted.takeRight(3).sum
  println(result)
}
