package io.github.avapl
package day11.puzzle2

import util.InputParser
import util.InputParser._

import scala.util.chaining._

object PuzzleInputParser extends InputParser[(IndexedSeq[Monkey], DivisorsProduct)](day = 11) {

  override protected def parse(string: String): (IndexedSeq[Monkey], DivisorsProduct) = {
    for {
      block <- string.splitBlocks.toIndexedSeq
    } yield parseMonkeyAndDivisor(block)
  }.unzip.pipe { case (monkeys, divisors) =>
    (monkeys, divisors.product)
  }

  private def parseMonkeyAndDivisor(monkeyBlock: String) = {
    val lines = monkeyBlock.splitLines
    val items = parseItems(lines)
    val inspectItem = parseInspectItem(lines)
    val divisor = parseDivisor(lines)
    val throwItemToMonkey = parseThrowItemToMonkey(divisor, lines)
    (Monkey(items, inspectItem, throwItemToMonkey), divisor)
  }

  private def parseItems(lines: Seq[String]) = {
    val s"  Starting items: $itemsString" = lines(1)
    itemsString.splitBy(", ").map(_.toLong)
  }

  private def parseInspectItem(lines: Seq[String]): WorryLevel => WorryLevel = {
    val s"  Operation: new = old $operatorString $valueString" = lines(2)
    (operatorString, valueString) match {
      case ("+", "old")     => old => math.addExact(old, old)
      case ("*", "old")     => old => math.multiplyExact(old, old)
      case ("+", intString) => math.addExact(_, intString.toInt)
      case ("*", intString) => math.multiplyExact(_, intString.toInt)
    }
  }

  private def parseDivisor(lines: Seq[String]) = {
    val s"  Test: divisible by $divisorString" = lines(3)
    divisorString.toInt
  }

  private def parseThrowItemToMonkey(divisor: Int, lines: Seq[String]): WorryLevel => Int = {
    val s"    If true: throw to monkey $ifTrueIndexString" = lines(4)
    val ifTrueIndex = ifTrueIndexString.toInt
    val s"    If false: throw to monkey $ifFalseIndexString" = lines(5)
    val ifFalseIndex = ifFalseIndexString.toInt
    worryLevel => if (worryLevel % divisor == 0) ifTrueIndex else ifFalseIndex
  }
}
