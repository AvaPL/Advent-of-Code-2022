package io.github.avapl

import scala.annotation.tailrec

package object day21 {

  sealed trait Monkey {
    def name: String
  }

  case class ValueMonkey(name: String, value: Long) extends Monkey

  case class OperationMonkey(
      name: String,
      leftMonkey: String,
      rightMonkey: String,
      operation: (Long, Long) => Long
  ) extends Monkey

  val rootMonkeyName = "root"

  def evaluateMonkeyValue(monkeys: Seq[Monkey], monkeyName: String): Long = {
    @tailrec
    def loop(remainingMonkeys: Seq[Monkey], monkeyNameToValue: Map[String, Long] = Map.empty): Long =
      if (remainingMonkeys.isEmpty)
        monkeyNameToValue(monkeyName)
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
