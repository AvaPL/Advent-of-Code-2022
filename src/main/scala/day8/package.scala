package io.github.avapl

package object day8 {
  type Trees = IndexedSeq[IndexedSeq[Int]]

  def indices(trees: Trees): Seq[(Int, Int)] =
    for {
      row <- trees.indices
      column <- trees.head.indices
    } yield (row, column)
}
