package io.github.avapl
package day1

object Puzzle1 extends App {
  val elfFoods = Day1InputParser.parsedInput
  val result = elfFoods.map(_.sum).max
  println(result)
}
