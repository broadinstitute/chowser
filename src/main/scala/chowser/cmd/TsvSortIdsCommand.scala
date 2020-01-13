package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class TsvSortIdsCommand(resourceConfig: ResourceConfig, inFile: InputId, outFile: OutputId, colName: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

