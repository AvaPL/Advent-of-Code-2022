package io.github.avapl
package util

import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

abstract class InputParser[T](day: Int) {
  protected def parse(string: String): T

  lazy val parsedInput: T = {
    val fileName = s"input/day$day.txt"
    val inputString = Using(Source.fromResource(fileName))(_.mkString).get
    parse(inputString)
  }
}

object InputParser {

  implicit class InputStringOps(val string: String) extends AnyVal {
    def splitByRegex(regex: String): Seq[String] =
      string.split(regex).toSeq

    def splitBy(delimiter: String): Seq[String] = {
      val quotedDelimiter = Regex.quote(delimiter)
      splitByRegex(quotedDelimiter)
    }

    def splitLines: Seq[String] =
      splitBy("\n")

    def splitBlocks: Seq[String] =
      splitBy("\n\n")
  }

}
