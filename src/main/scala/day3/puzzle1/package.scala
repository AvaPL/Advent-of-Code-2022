package io.github.avapl
package day3

package object puzzle1 {
  type Compartment = Set[ItemType]
  type Rucksack = (Compartment, Compartment)
}
