package io.github.avapl

package object day17 {

  sealed trait JetDirection
  case object Left extends JetDirection
  case object Right extends JetDirection

  type JetStream = Seq[JetDirection]

  type Shape = Seq[String]

  val shapes = Seq(
    Seq(
      ".......",
      ".......",
      ".......",
      "..####."
    ),
    Seq(
      ".......",
      "...#...",
      "..###..",
      "...#..."
    ),
    Seq(
      ".......",
      "....#..",
      "....#..",
      "..###.."
    ),
    Seq(
      "..#....",
      "..#....",
      "..#....",
      "..#...."
    ),
    Seq(
      ".......",
      ".......",
      "..##...",
      "..##..."
    )
  )
}
