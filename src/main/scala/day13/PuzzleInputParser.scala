package io.github.avapl
package day13

import util.InputParser
import util.InputParser._

import play.api.libs.json.{JsValue, Json}

object PuzzleInputParser extends InputParser[Seq[(JsValue, JsValue)]](day = 13) {

  override protected def parse(string: String): Seq[(JsValue, JsValue)] =
    for {
      block <- string.splitBlocks
      Seq(packet1, packet2) = block.splitLines.map(Json.parse)
    } yield (packet1, packet2)
}
