package io.github.avapl

package object day16 {

  type ValveName = String

  case class Valve(name: ValveName, flowRate: Int, tunnels: Set[ValveName])
}
