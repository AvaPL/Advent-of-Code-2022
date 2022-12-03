package io.github.avapl
package day3.puzzle2

import day3.calculatePriority

object Puzzle2 extends App {
  val rucksacks = Puzzle2InputParser.parsedInput
  val result = rucksacks
    .grouped(3)
    .map { case Seq(first, second, third) =>
      first.intersect(second).intersect(third).head
    }
    .map(calculatePriority)
    .sum
  println(result)
}
