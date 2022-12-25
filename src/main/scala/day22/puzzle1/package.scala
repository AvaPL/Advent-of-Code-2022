package io.github.avapl
package day22

package object puzzle1 {
  sealed abstract class Direction(val rowDirection: Int, val columnDirection: Int)
  case object Up extends Direction(-1, 0)
  case object Down extends Direction(1, 0)
  case object Left extends Direction(0, -1)
  case object Right extends Direction(0, 1)
}
