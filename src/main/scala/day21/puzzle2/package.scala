package io.github.avapl
package day21

package object puzzle2 {

  sealed trait Monkey {
    def name: String
  }

  case class ValueMonkey(name: String, value: Long) extends Monkey

  case class OperationMonkey(
      name: String,
      leftMonkey: String,
      rightMonkey: String,
      operation: (Long, Long) => Long
  ) extends Monkey

  val myName = "humn"
}
