package chowser.cmd

import chowser.util.io.IoId

case class TsvExtractUniqueCommand(inFile: IoId, outFile: IoId, colName: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

