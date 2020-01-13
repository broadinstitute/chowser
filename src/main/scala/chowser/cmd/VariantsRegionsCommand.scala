package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsRegionsCommand(resourceConfig: ResourceConfig, inFile: InputId, outFile: OutputId,
                                  chromColName: String, posColName: String, radius: Int)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
