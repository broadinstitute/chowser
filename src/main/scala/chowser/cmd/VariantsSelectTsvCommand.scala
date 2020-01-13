package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsSelectTsvCommand(resourceConfig: ResourceConfig, dataFile: InputId, selectionFile: InputId,
                                    outFile: OutputId, idColData: String, idColSelection: String)
  extends ChowserCommand with ChowserCommand.OneOutFile {

}
