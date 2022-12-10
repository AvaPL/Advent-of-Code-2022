package io.github.avapl

package object day10 {

  sealed trait Instruction
  case object Noop extends Instruction
  case class AddX(value: Int) extends Instruction

  def registerXValues(instructions: Seq[Instruction]): IndexedSeq[Int] = {
    val cycleToRegisterX = mapCycleToRegisterX(instructions)
    val totalCycles = cycleToRegisterX.keySet.max
    (0 to totalCycles).map { cycle =>
      cycleToRegisterX.getOrElse(cycle, cycleToRegisterX(cycle - 1))
    }
  }

  private def mapCycleToRegisterX(instructions: Seq[Instruction]) =
    instructions
      .scanLeft((0, 1)) {
        case ((cycle, registerX), Noop)        => (cycle + 1, registerX)
        case ((cycle, registerX), AddX(value)) => (cycle + 2, registerX + value)
      }
      .toMap
}
