package chowser.cmd

import better.files.File
import chowser.filter.Filter

case class TsvRangeCommand(inFile: File, outFile: File, colName: String, filter: Filter[Double])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

