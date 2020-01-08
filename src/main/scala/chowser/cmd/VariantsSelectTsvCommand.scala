package chowser.cmd

import chowser.util.io.IoId

case class VariantsSelectTsvCommand(dataFile: IoId, selectionFile: IoId, outFile: IoId,
                                    idColData: String, idColSelection: String)
  extends ChowserCommand with ChowserCommand.OneOutFile {

}
