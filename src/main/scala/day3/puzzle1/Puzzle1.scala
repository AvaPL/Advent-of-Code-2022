package io.github.avapl
package day3.puzzle1

import day3.calculatePriority

object Puzzle1 extends App {
  val rucksacks = Puzzle1InputParser.parsedInput
  val result = rucksacks
    .map { case (leftCompartment, rightCompartment) =>
      leftCompartment.intersect(rightCompartment).head
    }
    .map(calculatePriority)
    .sum
  println(result)
}
