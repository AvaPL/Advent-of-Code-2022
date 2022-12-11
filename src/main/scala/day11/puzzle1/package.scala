package io.github.avapl
package day11

package object puzzle1 {
  type WorryLevel = Int

  case class Monkey(
      items: Seq[WorryLevel],
      inspectItem: WorryLevel => WorryLevel,
      throwItemToMonkey: WorryLevel => Int
  )
}
