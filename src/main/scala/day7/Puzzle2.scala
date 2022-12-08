package io.github.avapl
package day7

object Puzzle2 extends App {
  val fileSystem = PuzzleInputParser.parsedInput
  val totalDiskSpace = 70_000_000
  val requiredFreeSpace = 30_000_000
  val fileSystemSize = directorySize(fileSystem)(pathToCheck = Vector("/"))
  val currentFreeSpace = totalDiskSpace - fileSystemSize
  val spaceToFreeUp = requiredFreeSpace - currentFreeSpace
  val result = fileSystem.keySet.toSeq
    .map(path => directorySize(fileSystem)(path))
    .sorted
    .find(_ > spaceToFreeUp)
    .get
  println(result)
}
