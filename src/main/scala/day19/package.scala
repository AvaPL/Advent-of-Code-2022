package io.github.avapl

package object day19 {

  case class RobotCost(ore: Int, clay: Int, obsidian: Int)

  case class Blueprint(
      id: Int,
      oreRobotCost: RobotCost,
      clayRobotCost: RobotCost,
      obsidianRobotCost: RobotCost,
      geodeRobotCost: RobotCost
  )

  def simulate(blueprint: Blueprint, totalMinutes: Int): Int = {
    var maxGeodes = 0

    def loop(
        ore: Int,
        clay: Int,
        obsidian: Int,
        geodes: Int,
        oreProduction: Int,
        clayProduction: Int,
        obsidianProduction: Int,
        geodesProduction: Int,
        minutesRemaining: Int
    ): Unit =
      if (minutesRemaining <= 0)
        maxGeodes = maxGeodes.max(geodes)
      else {
        val upperBound = calculateUpperBound(geodes, geodesProduction, minutesRemaining)
        if (upperBound > maxGeodes) {
          if (canAfford(blueprint.oreRobotCost)(ore, clay, obsidian))
            loop(
              ore = ore + oreProduction - blueprint.oreRobotCost.ore,
              clay = clay + clayProduction - blueprint.oreRobotCost.clay,
              obsidian = obsidian + obsidianProduction - blueprint.oreRobotCost.obsidian,
              geodes = geodes + geodesProduction,
              oreProduction = oreProduction + 1,
              clayProduction,
              obsidianProduction,
              geodesProduction,
              minutesRemaining = minutesRemaining - 1
            )
          if (canAfford(blueprint.clayRobotCost)(ore, clay, obsidian))
            loop(
              ore = ore + oreProduction - blueprint.clayRobotCost.ore,
              clay = clay + clayProduction - blueprint.clayRobotCost.clay,
              obsidian = obsidian + obsidianProduction - blueprint.clayRobotCost.obsidian,
              geodes = geodes + geodesProduction,
              oreProduction,
              clayProduction = clayProduction + 1,
              obsidianProduction,
              geodesProduction,
              minutesRemaining = minutesRemaining - 1
            )
          if (canAfford(blueprint.obsidianRobotCost)(ore, clay, obsidian))
            loop(
              ore = ore + oreProduction - blueprint.obsidianRobotCost.ore,
              clay = clay + clayProduction - blueprint.obsidianRobotCost.clay,
              obsidian = obsidian + obsidianProduction - blueprint.obsidianRobotCost.obsidian,
              geodes = geodes + geodesProduction,
              oreProduction,
              clayProduction,
              obsidianProduction = obsidianProduction + 1,
              geodesProduction,
              minutesRemaining = minutesRemaining - 1
            )
          if (canAfford(blueprint.geodeRobotCost)(ore, clay, obsidian))
            loop(
              ore = ore + oreProduction - blueprint.geodeRobotCost.ore,
              clay = clay + clayProduction - blueprint.geodeRobotCost.clay,
              obsidian = obsidian + obsidianProduction - blueprint.geodeRobotCost.obsidian,
              geodes = geodes + geodesProduction,
              oreProduction,
              clayProduction,
              obsidianProduction,
              geodesProduction = geodesProduction + 1,
              minutesRemaining = minutesRemaining - 1
            )
          loop(
            ore = ore + oreProduction,
            clay = clay + clayProduction,
            obsidian = obsidian + obsidianProduction,
            geodes = geodes + geodesProduction,
            oreProduction,
            clayProduction,
            obsidianProduction,
            geodesProduction = geodesProduction,
            minutesRemaining = minutesRemaining - 1
          )
        }
      }

    loop(
      ore = 0,
      clay = 0,
      obsidian = 0,
      geodes = 0,
      oreProduction = 1,
      clayProduction = 0,
      obsidianProduction = 0,
      geodesProduction = 0,
      minutesRemaining = totalMinutes
    )

    maxGeodes
  }

  // Not really efficient, can be optimized further
  private def calculateUpperBound(geodes: Int, geodesProduction: Int, minutesRemaining: Int) =
    geodes + Iterator.from(geodesProduction).take(minutesRemaining).sum

  private def canAfford(robotCost: RobotCost)(ore: Int, clay: Int, obsidian: Int) =
    robotCost.ore <= ore && robotCost.clay <= clay && robotCost.obsidian <= obsidian
}
