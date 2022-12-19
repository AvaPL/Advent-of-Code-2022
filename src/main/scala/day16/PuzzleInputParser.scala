package io.github.avapl
package day16

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Seq[Valve]](day = 16) {

  private val valveRegex = """Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r

  override protected def parse(string: String): Seq[Valve] =
    for {
      line <- string.splitLines
      valveRegex(name, flowRate, tunnelsString) = line
      tunnels = tunnelsString.splitBy(", ").toSet
    } yield Valve(name, flowRate.toInt, tunnels)
}
