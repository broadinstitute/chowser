package chowser.cmd

import better.files.File
import chowser.filter.Filter

case class TsvSliceCommand(inFile: File, outFile: File, colName: String, filter: Filter[String])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

