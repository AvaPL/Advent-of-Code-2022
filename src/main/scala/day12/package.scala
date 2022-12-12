package io.github.avapl

package object day12 {
  type Row = Int
  type Column = Int

  type Height = Char
  type HeightMap = IndexedSeq[IndexedSeq[Char]]

  type Marker = Char
  val startMarker = 'S'
  val endMarker = 'E'

  type Cost = Int
  type Costs = IndexedSeq[IndexedSeq[Cost]]

  def markerRowColumn(heightMap: HeightMap, marker: Marker): (Row, Column) = {
    val row = heightMap.indexWhere(_.contains(marker))
    val column = heightMap(row).indexOf(marker)
    (row, column)
  }

  def removeMarkers(heightMap: HeightMap): HeightMap =
    for {
      row <- heightMap
    } yield for {
      height <- row
    } yield height match {
      case `startMarker` => 'a'
      case `endMarker`   => 'z'
      case height        => height
    }
}
