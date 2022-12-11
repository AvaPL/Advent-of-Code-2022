package io.github.avapl
package day11.puzzle1

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[IndexedSeq[Monkey]](day = 11) {

  override protected def parse(string: String): IndexedSeq[Monkey] =
    for {
      block <- string.splitBlocks.toIndexedSeq
    } yield parseMonkey(block)

  private def parseMonkey(monkeyBlock: String) = {
    val lines = monkeyBlock.splitLines
    val items = parseItems(lines)
    val inspectItem = parseInspectItem(lines)
    val throwItemToMonkey = parseThrowItemToMonkey(lines)
    Monkey(items, inspectItem, throwItemToMonkey)
  }

  private def parseItems(lines: Seq[String]) = {
    val s"  Starting items: $itemsString" = lines(1)
    itemsString.splitBy(", ").map(_.toInt)
  }

  private def parseInspectItem(lines: Seq[String]): WorryLevel => WorryLevel = {
    val s"  Operation: new = old $operatorString $valueString" = lines(2)
    (operatorString, valueString) match {
      case ("+", "old")     => old => old + old
      case ("*", "old")     => old => old * old
      case ("+", intString) => _ + intString.toInt
      case ("*", intString) => _ * intString.toInt
    }
  }

  private def parseThrowItemToMonkey(lines: Seq[String]): WorryLevel => Int = {
    val s"  Test: divisible by $divisorString" = lines(3)
    val divisor = divisorString.toInt
    val s"    If true: throw to monkey $ifTrueIndexString" = lines(4)
    val ifTrueIndex = ifTrueIndexString.toInt
    val s"    If false: throw to monkey $ifFalseIndexString" = lines(5)
    val ifFalseIndex = ifFalseIndexString.toInt
    worryLevel => if (worryLevel % divisor == 0) ifTrueIndex else ifFalseIndex
  }
}
