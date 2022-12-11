package io.github.avapl
package day11.puzzle2

import com.softwaremill.quicklens._

import scala.annotation.tailrec
import scala.collection.mutable

object Puzzle2 extends App {
  val (monkeys, divisorsProduct) = PuzzleInputParser.parsedInput
  val inspectionMetrics = mutable.Map.empty[Int, Long].withDefaultValue(0)
  (0 until 10_000).foldLeft(monkeys) { case (monkeys, _) =>
    executeRound(monkeys)
  }
  val result = inspectionMetrics.values.toSeq.sorted.takeRight(2).product
  println(result)

  private def executeRound(monkeys: IndexedSeq[Monkey]) =
    monkeys.indices.foldLeft(monkeys) { case (monkeys, index) =>
      executeTurn(monkeys, index)
    }

  @tailrec
  private def executeTurn(monkeys: IndexedSeq[Monkey], index: Int): IndexedSeq[Monkey] =
    if (monkeys(index).items.isEmpty)
      monkeys
    else {
      recordItemInspection(index)
      val updatedMonkeys = inspectItem(monkeys, index)
      executeTurn(updatedMonkeys, index)
    }

  private def recordItemInspection(monkeyIndex: Int) =
    inspectionMetrics.put(monkeyIndex, inspectionMetrics(monkeyIndex) + 1)

  private def inspectItem(monkeys: IndexedSeq[Monkey], index: Int) = {
    val monkey = monkeys(index)
    val itemToInspect +: remainingItems = monkey.items
    val itemWithNewWorryLevel = monkey.inspectItem(itemToInspect) % divisorsProduct
    val throwMonkeyIndex = monkey.throwItemToMonkey(itemWithNewWorryLevel)
    monkeys
      .modify(_.at(index).items)
      .setTo(remainingItems)
      .modify(_.at(throwMonkeyIndex).items)
      .using(_ :+ itemWithNewWorryLevel)
  }
}
