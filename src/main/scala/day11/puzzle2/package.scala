package io.github.avapl
package day11

package object puzzle2 {
  type WorryLevel = Long
  type DivisorsProduct = Int

  case class Monkey(
      items: Seq[WorryLevel],
      inspectItem: WorryLevel => WorryLevel,
      throwItemToMonkey: WorryLevel => Int
  )
}
