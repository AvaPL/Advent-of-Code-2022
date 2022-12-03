package io.github.avapl

package object day3 {
  type ItemType = Char

  def calculatePriority(itemType: ItemType): Int =
    if (itemType.isLower)
      itemType - 'a' + 1
    else
      itemType - 'A' + 27
}
