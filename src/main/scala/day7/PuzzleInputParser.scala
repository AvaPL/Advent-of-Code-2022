package io.github.avapl
package day7

import util.InputParser
import util.InputParser._

object PuzzleInputParser extends InputParser[FileSystem](day = 7) {

  override protected def parse(string: String): FileSystem = {
    var fileSystem: FileSystem = Map.empty
    var currentPath: Path = Vector.empty

    def processCommand(command: String): Unit =
      command match {
        case "$ cd .."               => currentPath = currentPath.init
        case s"$$ cd $directoryName" => currentPath = currentPath :+ directoryName
        case ls =>
          val files = parseLsFiles(ls)
          fileSystem = fileSystem.updated(currentPath, files)
      }

    for (command <- string.splitByRegex("""\n(?=\$ )"""))
      processCommand(command)
    fileSystem
  }

  private val fileLineRegex = """(\d+) (.+)""".r

  private def parseLsFiles(nodesString: String) =
    nodesString.splitLines.collect { case fileLineRegex(fileSize, fileName) =>
      File(fileName, fileSize.toInt)
    }
}
