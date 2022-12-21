package io.github.avapl
package day21.puzzle2

import day21.rootMonkeyName
import util.InputParser
import util.InputParser._

import scala.annotation.tailrec

object PuzzleInputParser extends InputParser[Seq[Monkey]](day = 21) {

  override protected def parse(string: String): Seq[Monkey] = {
    val preprocessedInput = preprocessInput(string)
    for {
      line <- preprocessedInput.splitLines
    } yield parseMonkey(line)
  }

  private def preprocessInput(string: String) = {
    val lines = string.splitLines
    val (values, operations) = lines.partition(_.last.isDigit)
    val valuesWithoutHumn = values.filterNot(_.startsWith(myName))
    val operationsWithoutRoot = operations.filterNot(_.startsWith(rootMonkeyName))

    @tailrec
    def loop(lines: Seq[String], toReplace: Seq[String] = Seq(myName), replaced: Set[String] = Set.empty): Seq[String] =
      toReplace match {
        case Nil => lines
        case head +: tail =>
          var newToReplace = tail
          val newLines = lines.map {
            case line @ s"$value: $_" if replaced.contains(value) => line
            case s"$value: $left $operator $right" if left == head =>
              newToReplace = newToReplace ++ Seq(value, right)
              operator match {
                case "+" => s"$left: $value - $right"
                case "-" => s"$left: $value + $right"
                case "*" => s"$left: $value / $right"
                case "/" => s"$left: $value * $right"
              }
            case s"$value: $left $operator $right" if right == head =>
              newToReplace = newToReplace ++ Seq(value, left)
              operator match {
                case "+" => s"$right: $value - $left"
                case "-" => s"$right: $left - $value"
                case "*" => s"$right: $value / $left"
                case "/" => s"$right: $left / $value"
              }
            case line => line
          }
          loop(newLines, newToReplace.distinct, replaced + head)
      }

    val preprocessedOperations = loop(operationsWithoutRoot)
    val (rootLeftMonkey, rootRightMonkey) = operations
      .find(_.startsWith(rootMonkeyName))
      .map { case s"$_: $left $_ $right" =>
        (left, right)
      }
      .get
    val withRootMonkeysReplaced =
      if (preprocessedOperations.exists(_.startsWith(rootLeftMonkey)))
        preprocessedOperations.map(_.replace(rootRightMonkey, rootLeftMonkey))
      else
        preprocessedOperations.map(_.replace(rootLeftMonkey, rootRightMonkey))
    (valuesWithoutHumn ++ withRootMonkeysReplaced).mkString("\n")
  }

  private def parseMonkey(line: String) =
    line match {
      case s"$name: $leftMonkey $operator $rightMonkey" =>
        val operation: (Long, Long) => Long = operator match {
          case "+" => _ + _
          case "-" => _ - _
          case "*" => _ * _
          case "/" => _ / _
        }
        OperationMonkey(name, leftMonkey, rightMonkey, operation)
      case s"$name: $value" => ValueMonkey(name, value.toInt)
    }
}
