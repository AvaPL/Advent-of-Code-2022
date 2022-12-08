package io.github.avapl
package day8

object Puzzle2 extends App {
  val trees = PuzzleInputParser.parsedInput
  val result = indices(trees).map { case (row, column) =>
    scenicScore(row, column)
  }.max
  println(result)

  private def scenicScore(row: Int, column: Int) =
    Seq(
      topScenicScore _,
      bottomScenicScore _,
      leftScenicScore _,
      rightScenicScore _
    )
      .map(_(row, column))
      .product

  private def topScenicScore(row: Int, column: Int) = {
    var topScore = 0
    (0 until row).findLast { aboveIndex =>
      topScore += 1
      trees(aboveIndex)(column) >= trees(row)(column)
    }
    topScore
  }

  private def bottomScenicScore(row: Int, column: Int) = {
    var bottomScore = 0
    (row + 1 until trees.length).find { belowIndex =>
      bottomScore += 1
      trees(belowIndex)(column) >= trees(row)(column)
    }
    bottomScore
  }

  private def leftScenicScore(row: Int, column: Int) = {
    var leftScore = 0
    (0 until column).findLast { leftIndex =>
      leftScore += 1
      trees(row)(leftIndex) >= trees(row)(column)
    }
    leftScore
  }

  private def rightScenicScore(row: Int, column: Int) = {
    var rightScore = 0
    (column + 1 until trees.head.length).find { rightIndex =>
      rightScore += 1
      trees(row)(rightIndex) >= trees(row)(column)
    }
    rightScore
  }
}
