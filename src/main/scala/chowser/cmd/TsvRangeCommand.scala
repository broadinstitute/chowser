package chowser.cmd

import chowser.filter.Filter
import chowser.util.io.IoId

case class TsvRangeCommand(inFile: IoId, outFile: IoId, colName: String, filter: Filter[Double])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

