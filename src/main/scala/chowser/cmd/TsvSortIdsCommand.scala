package chowser.cmd

import better.files.File

case class TsvSortIdsCommand(inFile: File, outFile: File, colName: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

