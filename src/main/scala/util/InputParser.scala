package io.github.avapl
package util

import scala.util.matching.Regex

abstract class InputParser[T](day: Int) {
  protected def parse(string: String): T

  lazy val parsedInput: T = {
    val inputString = FileReader.readUnsafe(s"input/day$day/puzzle.txt")
    parse(inputString)
  }
}

object InputParser {

  implicit class InputStringOps(val string: String) extends AnyVal {
    def splitBy(delimiter: String): Seq[String] = {
      val quotedDelimiter = Regex.quote(delimiter)
      string.split(quotedDelimiter).toSeq
    }

    def splitByRegex(regex: String): Seq[String] =
      string.split(regex).toSeq

    def splitLines: Seq[String] =
      splitBy("\n")

    def splitBlocks: Seq[String] =
      splitBy("\n\n")
  }

}
