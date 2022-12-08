package io.github.avapl

package object day7 {
  type Path = Vector[String]

  case class File(name: String, size: Int)

  type FileSystem = Map[Path, Seq[File]]

  def directorySize(fileSystem: FileSystem)(pathToCheck: Path): Long =
    fileSystem.collect {
      case (path, files) if path.startsWith(pathToCheck) => files.map(_.size).sum
    }.sum
}
