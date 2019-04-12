package chowser.cmd

import better.files.File

case class TsvSortCommand(inFile: File, outFile: File, colName: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

