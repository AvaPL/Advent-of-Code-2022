package io.github.avapl

package object day5 {
  type Crate = Char
  type Stack[T] = List[T]

  case class RearrangementStep(
      quantity: Int,
      sourceStackNumber: Int,
      targetStackNumber: Int
  )
}
