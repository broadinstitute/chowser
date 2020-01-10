package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class VariantsSelectTsvCommand(dataFile: InputId, selectionFile: InputId, outFile: OutputId,
                                    idColData: String, idColSelection: String, keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneOutFile {

}
