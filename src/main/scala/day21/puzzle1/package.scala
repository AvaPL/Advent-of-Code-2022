package io.github.avapl
package day21

package object puzzle1 {

  sealed trait Monkey
  case class ValueMonkey(name: String, value: Long) extends Monkey
  case class OperationMonkey(name: String, leftMonkey: String, rightMonkey: String, operation: (Long, Long) => Long)
      extends Monkey
}
