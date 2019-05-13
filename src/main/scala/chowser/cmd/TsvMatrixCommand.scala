package chowser.cmd

import better.files.File

case class TsvMatrixCommand(valuesFile: File, idsFile: File, outFile: File,
                            idCol: String, idCol1: String, idCol2: String, valueCol: String)
  extends ChowserCommand with ChowserCommand.OneOutFile {
}

