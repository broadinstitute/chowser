package chowser.cmd

import chowser.filter.Filter
import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class TsvRangeCommand(resourceConfig: ResourceConfig, inFile: InputId, outFile: OutputId, colName: String,
                           filter: Filter[Double])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

