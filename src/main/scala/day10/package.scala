package io.github.avapl

package object day10 {

  sealed trait Instruction
  case object Noop extends Instruction
  case class AddX(value: Int) extends Instruction

  def mapCycleToRegisterX(instructions: Seq[Instruction]): Map[Int, Int] =
    instructions
      .scanLeft((0, 1)) {
        case ((cycle, registerX), Noop)        => (cycle + 1, registerX)
        case ((cycle, registerX), AddX(value)) => (cycle + 2, registerX + value)
      }
      .toMap
}
