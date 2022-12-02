package io.github.avapl
package day2

package object puzzle2 {

  sealed abstract class RoundResult(val score: Int)
  case object Lose extends RoundResult(score = 0)
  case object Draw extends RoundResult(score = 3)
  case object Win extends RoundResult(score = 6)

  type Round = (Shape, RoundResult)
  type StrategyGuide = Seq[Round]
}
