package io.github.avapl
package day21.puzzle1

import day21.rootMonkeyName

import scala.annotation.tailrec

object Puzzle1 extends App {
  val monkeys = PuzzleInputParser.parsedInput
  val result = evaluateRootMonkey(monkeys)
  println(result)

  private def evaluateRootMonkey(monkeys: Seq[Monkey]) = {
    @tailrec
    def loop(remainingMonkeys: Seq[Monkey], monkeyNameToValue: Map[String, Long] = Map.empty): Long =
      if (remainingMonkeys.isEmpty)
        monkeyNameToValue(rootMonkeyName)
      else {
        val (newRemainingMonkeys, newMonkeyNameToValue) = evaluateMonkeys(remainingMonkeys, monkeyNameToValue)
        loop(newRemainingMonkeys, newMonkeyNameToValue)
      }

    loop(monkeys)
  }

  private def evaluateMonkeys(remainingMonkeys: Seq[Monkey], monkeyNameToValue: Map[String, Long]) =
    remainingMonkeys.foldLeft((Seq.empty[Monkey], monkeyNameToValue)) {
      case ((remainingMonkeys, monkeyNameToValue), ValueMonkey(name, value)) =>
        (remainingMonkeys, monkeyNameToValue + (name -> value))
      case (
            (remainingMonkeys, monkeyNameToValue),
            monkey @ OperationMonkey(name, leftMonkey, rightMonkey, operation)
          ) =>
        val value = for {
          leftMonkeyValue <- monkeyNameToValue.get(leftMonkey)
          rightMonkeyValue <- monkeyNameToValue.get(rightMonkey)
        } yield operation(leftMonkeyValue, rightMonkeyValue)
        value match {
          case Some(value) =>
            (remainingMonkeys, monkeyNameToValue + (name -> value))
          case None =>
            (remainingMonkeys :+ monkey, monkeyNameToValue)
        }
    }
}
