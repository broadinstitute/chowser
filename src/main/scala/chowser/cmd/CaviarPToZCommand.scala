package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class CaviarPToZCommand(resourceConfig: ResourceConfig,
                             inFile: InputId, outFile: OutputId, idCol: String, pCol: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

