package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsSelectVcfCommand(resourceConfig: ResourceConfig, dataFile: InputId, selectionFile: InputId,
                                    outFile: OutputId, idColSelection: String)
  extends ChowserCommand with ChowserCommand.OneOutFile {

}
