package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class VariantsSelectVcfCommand(dataFile: InputId, selectionFile: InputId, outFile: OutputId,
                                    idColSelection: String, keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneOutFile {

}
