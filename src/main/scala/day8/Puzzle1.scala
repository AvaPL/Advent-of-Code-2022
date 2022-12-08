package io.github.avapl
package day8

object Puzzle1 extends App {
  val trees = PuzzleInputParser.parsedInput
  val result = indices(trees).count { case (row, column) =>
    !isHidden(row, column)
  }
  println(result)

  private def isHidden(row: Int, column: Int) =
    Seq(
      isHiddenFromTop _,
      isHiddenFromBottom _,
      isHiddenFromLeft _,
      isHiddenFromRight _
    )
      .forall(_(row, column))

  private def isHiddenFromTop(row: Int, column: Int) =
    (0 until row).exists { aboveIndex =>
      trees(aboveIndex)(column) >= trees(row)(column)
    }

  private def isHiddenFromBottom(row: Int, column: Int) =
    (row + 1 until trees.length).exists { belowIndex =>
      trees(belowIndex)(column) >= trees(row)(column)
    }

  private def isHiddenFromLeft(row: Int, column: Int) =
    (0 until column).exists { leftIndex =>
      trees(row)(leftIndex) >= trees(row)(column)
    }

  private def isHiddenFromRight(row: Int, column: Int) =
    (column + 1 until trees.head.length).exists { rightIndex =>
      trees(row)(rightIndex) >= trees(row)(column)
    }
}
