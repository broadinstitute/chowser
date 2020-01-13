package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class TsvMatrixCommand(resourceConfig: ResourceConfig, valuesFile: InputId, idsFile: InputId, outFile: OutputId,
                            idCol1: String, idCol2: String, valueCol: String)
  extends ChowserCommand with ChowserCommand.OneOutFile {
}

