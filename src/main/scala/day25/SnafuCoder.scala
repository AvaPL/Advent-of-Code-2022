package io.github.avapl
package day25

import scala.math.{pow, round}

object SnafuCoder {

  def decode(snafuNumber: SnafuNumber): Long =
    snafuNumber.reverse.zipWithIndex.map { case (snafuDigit, exponent) =>
      val digit = decodeSnafuDigit(snafuDigit)
      digit * powerOf5(exponent)
    }.sum

  private def decodeSnafuDigit(snafuDigit: Char) =
    snafuDigit match {
      case '=' => -2
      case '-' => -1
      case '0' => 0
      case '1' => 1
      case '2' => 2
    }

  private def powerOf5(exponent: Int) =
    round(pow(5, exponent))

  def encode(number: Long): SnafuNumber =
    List
      .unfold(number) { number =>
        Option.when(number != 0) {
          val modulo5 = number % 5
          val shiftedBy2DividedBy5 = (number + 2) / 5
          (encodeRemainder(modulo5), shiftedBy2DividedBy5)
        }
      }
      .mkString
      .reverse

  private def encodeRemainder(remainder: Long) =
    remainder match {
      case 0 => '0'
      case 1 => '1'
      case 2 => '2'
      case 3 => '='
      case 4 => '-'
    }
}
