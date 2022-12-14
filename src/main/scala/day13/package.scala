package io.github.avapl

import play.api.libs.json.{JsArray, JsNumber, JsValue}

package object day13 {
  def comparePacketPair(packet1: JsValue, packet2: JsValue): Int =
    (packet1, packet2) match {
      case (JsArray(Nil), JsArray(Nil)) => 0
      case (JsArray(Nil), _)            => -1
      case (_, JsArray(Nil))            => 1
      case (JsArray(array1Head +: array1Tail), JsArray(array2Head +: array2Tail)) =>
        val headComparison = comparePacketPair(array1Head, array2Head)
        if (headComparison == 0) comparePacketPair(JsArray(array1Tail), JsArray(array2Tail))
        else headComparison
      case (array: JsArray, number: JsNumber)     => comparePacketPair(array, JsArray(Seq(number)))
      case (number: JsNumber, array: JsArray)     => comparePacketPair(JsArray(Seq(number)), array)
      case (JsNumber(number1), JsNumber(number2)) => number1.compare(number2)
    }
}
