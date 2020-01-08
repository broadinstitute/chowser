package chowser.cmd

import chowser.util.io.IoId

case class VariantsSelectVcfCommand(dataFile: IoId, selectionFile: IoId, outFile: IoId, idColSelection: String)
  extends ChowserCommand with ChowserCommand.OneOutFile {

}
