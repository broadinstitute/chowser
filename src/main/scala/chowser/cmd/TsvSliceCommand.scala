package chowser.cmd

import chowser.filter.Filter
import chowser.util.io.IoId

case class TsvSliceCommand(inFile: IoId, outFile: IoId, colName: String, filter: Filter[String])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

