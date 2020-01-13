package chowser.cmd

import chowser.filter.Filter
import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class TsvSliceCommand(resourceConfig: ResourceConfig, inFile: InputId, outFile: OutputId, colName: String,
                           filter: Filter[String])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

