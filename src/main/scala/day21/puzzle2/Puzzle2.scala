package io.github.avapl
package day21.puzzle2

import scala.annotation.tailrec

object Puzzle2 extends App {
  val monkeys = PuzzleInputParser.parsedInput
  val result = evaluateMyNumber(monkeys)
  println(result)

  private def evaluateMyNumber(monkeys: Seq[Monkey]) = {
    @tailrec
    def loop(remainingMonkeys: Seq[Monkey], monkeyNameToValue: Map[String, Long] = Map.empty): Long =
      if (remainingMonkeys.isEmpty)
        monkeyNameToValue(myName)
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
