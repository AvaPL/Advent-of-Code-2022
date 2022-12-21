package io.github.avapl
package day21.puzzle2

import day21.{Monkey, OperationMonkey, ValueMonkey, rootMonkeyName}
import util.InputParser
import util.InputParser._

import scala.annotation.tailrec
import scala.util.chaining._

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
    val (rootLeftMonkey, rootRightMonkey) = rootEqualMonkeys(operations)
    val preprocessedValues = values.pipe(removeMyName)
    val preprocessedOperations = operations
      .pipe(removeRootMonkey)
      .pipe(moveUnknownsToLeft)
      .pipe(replaceRootMonkeys(rootRightMonkey, rootLeftMonkey))
    (preprocessedValues ++ preprocessedOperations).mkString("\n")
  }

  private def rootEqualMonkeys(operations: Seq[String]) =
    operations
      .find(_.startsWith(rootMonkeyName))
      .map { case s"$_: $left $_ $right" =>
        (left, right)
      }
      .get

  private def removeMyName(values: Seq[String]) =
    values.filterNot(_.startsWith(myName))

  private def removeRootMonkey(operations: Seq[String]) =
    operations.filterNot(_.startsWith(rootMonkeyName))

  private def moveUnknownsToLeft(lines: Seq[String]) = {
    @tailrec
    def loop(lines: Seq[String], toReplace: Seq[String] = Seq(myName), replaced: Set[String] = Set.empty): Seq[String] =
      toReplace match {
        case Nil => lines
        case head +: tail =>
          var newToReplace = tail
          val newLines = lines.map {
            case line @ s"$value: $_" if replaced.contains(value) => line
            case s"$value: $left $operator $right" if left == head =>
              newToReplace ++= Seq(value, right)
              withLeftAsUnknown(value, left, operator, right)
            case s"$value: $left $operator $right" if right == head =>
              newToReplace ++= Seq(value, left)
              withRightAsUnknown(value, left, operator, right)
            case line => line
          }
          loop(newLines, newToReplace.distinct, replaced + head)
      }

    loop(lines)
  }

  private def withLeftAsUnknown(value: String, left: String, operator: String, right: String) =
    operator match {
      case "+" => s"$left: $value - $right"
      case "-" => s"$left: $value + $right"
      case "*" => s"$left: $value / $right"
      case "/" => s"$left: $value * $right"
    }

  private def withRightAsUnknown(value: String, left: String, operator: String, right: String) =
    operator match {
      case "+" => s"$right: $value - $left"
      case "-" => s"$right: $left - $value"
      case "*" => s"$right: $value / $left"
      case "/" => s"$right: $left / $value"
    }

  private def replaceRootMonkeys(rootRightMonkey: String, rootLeftMonkey: String)(operations: Seq[String]) = {
    val (from, to) =
      if (operations.exists(_.startsWith(rootLeftMonkey)))
        (rootRightMonkey, rootLeftMonkey)
      else
        (rootLeftMonkey, rootRightMonkey)
    operations.map(_.replace(from, to))
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
