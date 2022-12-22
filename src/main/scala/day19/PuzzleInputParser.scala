package io.github.avapl
package day19

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[Seq[Blueprint]](day = 19) {

  override protected def parse(string: String): Seq[Blueprint] =
    for {
      line <- string.splitLines
      s"Blueprint $id: Each ore robot costs $oreRobotOre ore. Each clay robot costs $clayRobotOre ore. Each obsidian robot costs $obsidianRobotOre ore and $obsidianRobotClay clay. Each geode robot costs $geodeRobotOre ore and $geodeRobotObsidian obsidian." =
        line
    } yield {
      val oreRobotCost = RobotCost(ore = oreRobotOre.toInt, clay = 0, obsidian = 0)
      val clayRobotCost = RobotCost(ore = clayRobotOre.toInt, clay = 0, obsidian = 0)
      val obsidianRobotCost = RobotCost(ore = obsidianRobotOre.toInt, clay = obsidianRobotClay.toInt, obsidian = 0)
      val geodeRobotCost = RobotCost(ore = geodeRobotOre.toInt, clay = 0, obsidian = geodeRobotObsidian.toInt)
      Blueprint(id.toInt, oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost)
    }
}
